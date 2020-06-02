;;; rap-client.el --- REPL Adapter Protocol mode      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ruin0x11

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Ruin0x11 <ipickering2@gmail.com>
;; Keywords: languages, debug
;; URL: https://github.com/Ruin0x11/rap-mode
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (rap-mode "6.0") (dash-functional "1.2.0") (bui "1.1.0") (f "0.20.0") (s "1.12.0") (rap-treemacs "0.1"))
;; Version: 0.3

;;; Commentary:
;; REPL Adapter Protocol client for Emacs.

;;; Code:

(require 'ht)

(defvar rap--cancelable-requests (ht))

(cl-defstruct rap--client
  (language-id nil)
  (notification-handlers (make-hash-table :test 'equal))
  (request-handlers (make-hash-table :test 'equal))
  (response-handlers (make-hash-table :test 'eql))
  server-id)

(defun rap-workspace-status (status-string &optional workspace)
  "Set current workspace status to STATUS-STRING.
If WORKSPACE is not specified defaults to rap--cur-workspace."
  (let ((status-string (when status-string (replace-regexp-in-string "%" "%%" status-string))))
    (setf (rap--workspace-status-string (or workspace rap--cur-workspace)) status-string)))

(defun rap-session-set-metadata (key value &optional _workspace)
  "Associate KEY with VALUE in the WORKSPACE metadata.
If WORKSPACE is not provided current workspace will be used."
  (puthash key value (rap-session-metadata (rap-session))))

(defalias 'rap-workspace-set-metadata 'rap-session-set-metadata)

(defun rap-session-get-metadata (key &optional _workspace)
  "Lookup KEY in WORKSPACE metadata.
If WORKSPACE is not provided current workspace will be used."
  (gethash key (rap-session-metadata (rap-session))))

(defalias 'rap-workspace-get-metadata 'rap-session-get-metadata)

(defun rap--make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (list :jsonrpc "2.0" :method method :params params))

(defun rap--make-request (method &optional params)
  "Create request body for method METHOD and parameters PARAMS."
  (rap--make-notification method params))

(defalias 'rap-make-request 'rap--make-request)

(defun rap--make-response (request result)
  "Create response for REQUEST with RESULT."
  `(:jsonrpc "2.0" :id ,(gethash "id" request) :result ,result))

(defun rap-make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (rap--make-notification method params))

(defconst rap--default-notification-handlers
  (ht ("telemetry/event" #'ignore)))

(defun rap--on-notification (workspace notification)
  "Call the appropriate handler for NOTIFICATION."
  (-let (((&hash "params" "method") notification)
         (client (rap--workspace-client workspace)))
    (when rap-log-io
      (rap--log-entry-new (rap--make-log-entry method nil params 'incoming-notif)
                          rap--cur-workspace))
    (if-let (handler (or (gethash method (rap--client-notification-handlers client))
                         (gethash method rap--default-notification-handlers)))
        (funcall handler workspace params)
      (unless (string-prefix-p "$" method)
        (rap-warn "Unknown notification: %s" method)))))

(defun rap--send-no-wait (message proc)
  "Send MESSAGE to PROC without waiting for further output."
  (condition-case err
      (process-send-string proc message)
    ('error (rap--error "Sending to process failed with the following error: %s"
                        (error-message-string err)))))

(define-error 'rap-parse-error
  "Error parsing message from language server" 'rap-error)
(define-error 'rap-unknown-message-type
  "Unknown message type" '(rap-error rap-parse-error))
(define-error 'rap-unknown-json-rpc-version
  "Unknown JSON-RPC protocol version" '(rap-error rap-parse-error))
(define-error 'rap-no-content-length
  "Content-Length header missing in message" '(rap-error rap-parse-error))
(define-error 'rap-invalid-header-name
  "Invalid header name" '(rap-error rap-parse-error))

;;  id  method
;;   x    x     request
;;   x    .     response
;;   .    x     notification
(defun rap--get-message-type (json-data)
  "Get the message type from JSON-DATA."
  (when (not (string= (gethash "jsonrpc" json-data "") "2.0"))
    (signal 'rap-unknown-json-rpc-version (list (gethash "jsonrpc" json-data))))
  (if (gethash "id" json-data nil)
      (if (gethash "error" json-data nil)
          'response-error
        (if (gethash "method" json-data nil)
            'request
          'response))
    (if (gethash "method" json-data nil)
        'notification
      (signal 'rap-unknown-message-type (list json-data)))))

(defun rap--parser-on-message (json-data workspace)
  "Called when JSON-DATA is read by WORKSPACE from the server."
  (with-demoted-errors "Error processing message %S."
    (with-rap-workspace workspace
      (let* ((client (rap--workspace-client workspace))
             (received-time (current-time))
             (server-id (rap--client-server-id client))
             (after-parsed-time (current-time))
             (id (--when-let (gethash "id" json-data)
                   (if (stringp it) (string-to-number it) it)))
             (data (gethash "result" json-data)))
        (pcase (rap--get-message-type json-data)
          ('response
           (cl-assert id)
           (-let [(callback _ method start-time before-send) (gethash id (rap--client-response-handlers client))]
             (when rap-log-io
               (rap--log-entry-new
                (rap--make-log-entry method id data 'incoming-resp
                                     (/ (nth 2 (time-since before-send)) 1000))
                workspace))
             (when callback
               (funcall callback (gethash "result" json-data))
               (remhash id (rap--client-response-handlers client))
               (rap--log-request-time server-id method id start-time before-send
                                      received-time after-parsed-time (current-time)))))
          ('response-error
           (cl-assert id)
           (-let [(_ callback method start-time before-send) (gethash id (rap--client-response-handlers client))]
             (when rap-log-io
               (rap--log-entry-new
                (rap--make-log-entry method id data 'incoming-resp
                                     (/ (nth 2 (time-since before-send)) 1000))
                workspace))
             (when callback
               (funcall callback (gethash "error" json-data))
               (remhash id (rap--client-response-handlers client))
               (rap--log-request-time server-id method id start-time before-send
                                      received-time after-parsed-time (current-time)))))
          ('notification
           (let ((before-notification (current-time)))
             (rap--on-notification workspace json-data)
             (rap--log-notification-performance
              server-id json-data received-time after-parsed-time before-notification (current-time))))
          ('request (rap--on-request workspace json-data)))))))

(defun rap--send-request-response (workspace recv-time request response)
  "Send the RESPONSE for REQUEST in WORKSPACE and log if needed."
  (-let* (((&hash "params" "method" "id") request)
          (process (rap--workspace-proc workspace))
          (response (rap--make-response request response))
          (req-entry (and rap-log-io
                          (rap--make-log-entry method id params 'incoming-req)))
          (resp-entry (and rap-log-io
                           (rap--make-log-entry method id response 'outgoing-resp
                                                (/ (nth 2 (time-since recv-time)) 1000)))))
    ;; Send response to the server.
    (when rap-log-io
      (rap--log-entry-new req-entry workspace)
      (rap--log-entry-new resp-entry workspace))
    (rap--send-no-wait (rap--make-message response) process)))

(defun rap--on-request (workspace request)
  "Call the appropriate handler for REQUEST, and send the return value to the server.
WORKSPACE is the active workspace."
  (-let* ((recv-time (current-time))
          ((&hash "params" "method") request)
          (client (rap--workspace-client workspace))
          handler
          (response (cond
                     ((setq handler (gethash method (rap--client-request-handlers client) nil))
                      (funcall handler workspace params))
                     ((setq handler (gethash method (rap--client-async-request-handlers client) nil))
                      (funcall handler workspace params
                               (-partial #'rap--send-request-response
                                         workspace recv-time request))
                      'delay-response)
                     (t (rap-warn "Unknown request method: %s" method) nil))))
    ;; Send response to the server.
    (unless (eq response 'delay-response)
      (rap--send-request-response workspace recv-time request response))))

(defconst rap--errors
  '((-32700 "Parse Error")
    (-32600 "Invalid Request")
    (-32601 "Method not Found")
    (-32602 "Invalid Parameters")
    (-32603 "Internal Error")
    (-32099 "Server Start Error")
    (-32000 "Server End Error")
    (-32002 "Server Not Initialized")
    (-32001 "Unknown Error Code")
    (-32800 "Request Cancelled"))
  "Alist of error codes to user friendly strings.")

(defun rap--error-string (err)
  "Format ERR as a user friendly string."
  (let ((code (gethash "code" err))
        (message (gethash "message" err)))
    (format "Error from the Language Server: %s (%s)"
            message
            (or (car (alist-get code rap--errors)) "Unknown error"))))

(defun rap--get-body-length (headers)
  (let ((content-length (cdr (assoc "Content-Length" headers))))
    (if content-length
        (string-to-number content-length)

      ;; This usually means either the server or our parser is
      ;; screwed up with a previous Content-Length
      (error "No Content-Length header"))))

(defun rap--parse-header (s)
  "Parse string S as a RAP (KEY . VAL) header."
  (let ((pos (string-match "\:" s))
        key val)
    (unless pos
      (signal 'rap-invalid-header-name (list s)))
    (setq key (substring s 0 pos)
          val (s-trim-left (substring s (+ 1 pos))))
    (when (string-equal key "Content-Length")
      (cl-assert (cl-loop for c across val
                          when (or (> c ?9) (< c ?0)) return nil
                          finally return t)
                 nil (format "Invalid Content-Length value: %s" val)))
    (cons key val)))

(defmacro rap--read-json (str)
  "Read json string STR."
  (if (progn
        (require 'json)
        (fboundp 'json-parse-string))
      `(json-parse-string ,str
                          :object-type 'hash-table
                          :null-object nil
                          :false-object nil)
    `(let ((json-array-type 'vector)
           (json-object-type 'hash-table)
           (json-false nil))
       (json-read-from-string ,str))))

(defun rap--json-pretty-print (msg)
  "Convert json MSG string to pretty printed json string."
  (let ((json-encoding-pretty-print t))
    (json-encode (json-read-from-string msg))))

(defun rap--create-filter-function (workspace)
  "Make filter for the workspace."
  (let ((body-received 0)
        leftovers body-length body chunk)
    (lambda (_proc input)
      (setf chunk (concat leftovers (with-no-warnings (string-as-unibyte input))))
      (while (not (string= chunk ""))
        (if (not body-length)
            ;; Read headers
            (if-let (body-sep-pos (string-match-p "\r\n\r\n" chunk))
                ;; We've got all the headers, handle them all at once:
                (setf body-length (rap--get-body-length
                                   (mapcar #'rap--parse-header
                                           (split-string
                                            (substring chunk
                                                       (or (string-match-p "Content-Length" chunk)
                                                           (error "Unable to find Content-Length header."))
                                                       body-sep-pos)
                                            "\r\n")))
                      body-received 0
                      leftovers nil
                      chunk (substring chunk (+ body-sep-pos 4)))

              ;; Haven't found the end of the headers yet. Save everything
              ;; for when the next chunk arrives and await further input.
              (setf leftovers chunk
                    chunk ""))
          (let* ((chunk-length (string-bytes chunk))
                 (left-to-receive (- body-length body-received))
                 (this-body (if (< left-to-receive chunk-length)
                                (prog1 (substring chunk 0 left-to-receive)
                                  (setf chunk (substring chunk left-to-receive)))
                              (prog1 chunk
                                (setf chunk ""))))
                 (body-bytes (string-bytes this-body)))
            (push this-body body)
            (setf body-received (+ body-received body-bytes))
            (when (>= chunk-length left-to-receive)
              (rap--parser-on-message
               (condition-case err
                   (let ((parsed-message (decode-coding-string
                                          (apply #'concat
                                                 (nreverse
                                                  (prog1 body
                                                    (setf leftovers nil
                                                          body-length nil
                                                          body-received nil
                                                          body nil)))) 'utf-8) ))
                     (rap--read-json parsed-message))
                 (error
                  (rap-warn "Failed to parse the following chunk:\n'''\n%s\n'''\nwith message %s"
                            (concat leftovers input)
                            err)))
               workspace))))))))

(defun rap--send-notification (body)
  "Send BODY as a notification to the language server."
  (rap-foreach-workspace
   (when rap-log-io
     (rap--log-entry-new (rap--make-log-entry
                          (plist-get body :method)
                          nil (plist-get body :params) 'outgoing-notif)
                         rap--cur-workspace))
   (rap--send-no-wait (rap--make-message body)
                      (rap--workspace-proc rap--cur-workspace))))

(defalias 'rap-send-notification 'rap--send-notification)

(defun rap-notify (method params)
  "Send notification METHOD with PARAMS."
  (rap--send-notification (rap--make-notification method params)))

(defun rap--cur-workspace-check ()
  "Check whether buffer rap workspace(s) are set."
  (cl-assert (rap-workspaces) nil
             "No language server(s) is associated with this buffer."))

(defun rap--send-request (body &optional no-wait no-merge)
  "Send BODY as a request to the language server, get the response.
If NO-WAIT is non-nil, don't synchronously wait for a response.
If NO-MERGE is non-nil, don't merge the results but return alist workspace->result."
  (rap-request (plist-get body :method)
               (plist-get body :params)
               :no-wait no-wait
               :no-merge no-merge))

(defalias 'rap-send-request 'rap--send-request
  "Send BODY as a request to the language server and return the response synchronously.
\n(fn BODY)")

(cl-defun rap-request (method params &key no-wait no-merge)
  "Send request METHOD with PARAMS.
If NO-MERGE is non-nil, don't merge the results but return alist workspace->result.
If NO-WAIT is non-nil send the request as notification."
  (if no-wait
      (rap-notify method params)
    (let* ((send-time (time-to-seconds (current-time)))
           ;; max time by which we must get a response
           (expected-time (+ send-time rap-response-timeout))
           resp-result resp-error)
      (unwind-protect
          (progn
            (rap-request-async method params (lambda (res) (setf resp-result (or res :finished)))
                               :error-handler (lambda (err) (setf resp-error err))
                               :no-merge no-merge
                               :mode 'detached
                               :cancel-token :sync-request)
            (while (not (or resp-error resp-result))
              (accept-process-output nil 0.001)
              (when (< expected-time (time-to-seconds (current-time)))
                (error "Timeout while waiting for response. Method: %s." method)))

            (cond
             ((eq resp-result :finished) nil)
             (resp-result resp-result)
             ((ht? resp-error) (error (gethash "message" resp-error)))
             (t (error (gethash "message" (cl-first resp-error))))))
        (rap-cancel-request-by-token :sync-request)))))

(cl-defun rap-request-while-no-input (method params)
  "Send request METHOD with PARAMS and waits until there is no input."
  (let* (resp-result resp-error)
    (unwind-protect
        (progn
          (rap-request-async
           method
           params
           (lambda (res) (setf resp-result (or res :finished)))
           :error-handler (lambda (err) (setf resp-error err))
           :mode 'detached
           :cancel-token :sync-request)

          (while (and (not (or resp-error resp-result))
                      (not (input-pending-p)))
            (accept-process-output nil 0.001))
          (cond
           ((eq resp-result :finished) nil)
           (resp-result resp-result)
           ((ht? resp-error) (error (gethash "message" resp-error)))
           ((input-pending-p) nil)
           (t (error (gethash "message" (cl-first resp-error))))))
      (rap-cancel-request-by-token :sync-request))))

(defun rap--merge-results (results method)
  "Merge RESULTS by filtering the empty hash-tables and merging the lists.
METHOD is the executed method so the results could be merged
depending on it."
  (pcase (--map (if (vectorp it) (append it nil) it) (-filter 'identity results))
    (`() ())
    ;; only one result - simply return it
    (`(,fst) fst)
    ;; multiple results merge it based on strategy
    (results
     (pcase method
       (_ (apply 'append (seq-map (lambda (it)
                                    (if (seqp it)
                                        it
                                      (list it)))
                                  results)))))))

(cl-defun rap-request-async (method params callback &key mode error-handler no-merge cancel-token)
  "Send request METHOD with PARAMS."
  (rap--send-request-async `(:jsonrpc "2.0" :method ,method :params ,params) callback mode error-handler no-merge cancel-token))

(defun rap--create-async-callback (count callback mode method no-merge cancel-token)
  "Create async handler expecting COUNT results, merge them and call CALLBACK.
MODE determines when the callback will be called depending on the
condition of the original buffer. METHOD is the invoked method.
If NO-MERGE is non-nil, don't merge the results but return alist workspace->result."
  (let ((buf (current-buffer))
        results)
    (cl-labels ((handle-result
                 ()
                 (when cancel-token
                   (remhash cancel-token rap--cancelable-requests))
                 (funcall callback (if no-merge
                                       results
                                     (rap--merge-results (-map #'cl-rest results) method)))))
      (pcase mode
        ('detached (lambda (result)
                     (push (cons rap--cur-workspace result) results)
                     (when (and (eq (length results) count))
                       (handle-result))))
        ('alive (lambda (result)
                  (push (cons rap--cur-workspace result) results)
                  (if (and (eq (length results) count)
                           (buffer-live-p buf))
                      (with-current-buffer buf
                        (handle-result))
                    (rap-log "Buffer is not alive ignoring response. Method %s." method))))
        ('tick (let ((tick (buffer-chars-modified-tick)))
                 (lambda (result)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (if (and (= tick (buffer-chars-modified-tick)))
                           (progn
                             (push (cons rap--cur-workspace result)  results)
                             (when (eq (length results) count)
                               (handle-result)))
                         (rap-log "Buffer modified ignoring response. Method %s." method)))))))
        (_ (lambda (result)
             (push (cons rap--cur-workspace result) results)
             (if (and (eq (length results) count)
                      (eq buf (current-buffer)))
                 (handle-result)
               (rap-log "Buffer switched - ignoring response. Method %s" method))))))))

(defun rap--create-default-error-handler (method)
  "Default error handler.
METHOD is the executed method."
  (lambda (error)
    (rap--warn "%s" (or (gethash "message" error)
                        (format "%s Request has failed" method)))))

(defvar rap--cancelable-requests (ht))

(defun rap-cancel-request-by-token (cancel-token)
  "Cancel request using CANCEL-TOKEN."
  (-when-let ((request-id . workspaces) (gethash cancel-token rap--cancelable-requests))
    (with-rap-workspaces workspaces
      (rap--cancel-request request-id))
    (remhash cancel-token rap--cancelable-requests)))

(defun rap--send-request-async (body callback &optional mode error-callback no-merge cancel-token)
  "Send BODY as a request to the language server.
Call CALLBACK with the response received from the server
asynchronously. MODE determines when the callback will be called
depending on the condition of the original buffer. It could be:
`detached' which means that the callback will be executed no
matter what has happened to the buffer. `alive' - the callback
will be executed only if the buffer from which the call was
executed is still alive. `current' the callback will be executed
only if the original buffer is still selected. `tick' - the
callback will be executed only if the buffer was not modified.

ERROR-CALLBACK will be called in case the request has failed.
If NO-MERGE is non-nil, don't merge the results but return alist workspace->result."
  (when cancel-token
    (rap-cancel-request-by-token cancel-token))

  (if-let ((target-workspaces (rap--find-workspaces-for body)))
      (let* ((start-time (current-time))
             (method (plist-get body :method))
             (workspaces-count (length target-workspaces))
             (async-callback (rap--create-async-callback workspaces-count
                                                         callback
                                                         mode
                                                         method
                                                         no-merge
                                                         cancel-token))
             (error-async-callback (rap--create-async-callback
                                    workspaces-count
                                    (or error-callback
                                        (rap--create-default-error-handler method))
                                    mode
                                    method
                                    nil
                                    cancel-token))
             (id (cl-incf rap-last-id))
             (body (plist-put body :id id)))
        (when cancel-token
          (puthash cancel-token (cons id target-workspaces) rap--cancelable-requests))
        (seq-doseq (workspace target-workspaces)
          (with-rap-workspace workspace
            (when rap-log-io
              (rap--log-entry-new (rap--make-log-entry method id
                                                       (plist-get body :params)
                                                       'outgoing-req)
                                  workspace))
            (let ((message (rap--make-message body)))
              (puthash id
                       (list async-callback error-async-callback method start-time (current-time))
                       (-> rap--cur-workspace
                           rap--workspace-client
                           rap--client-response-handlers))
              (rap--send-no-wait message (rap--workspace-proc workspace)))))
        body)
    (error "The connected server(s) does not support method %s
To find out what capabilities support your server use `M-x rap-describe-session' and expand the capabilities section."
           (plist-get body :method))))

(defun rap--client-capabilities (&optional custom-capabilities)
  "Return the client capabilities."
  (append '() (custom-capabilities)))

(defun rap--start-connection (session client project-root)
  "Initiates connection created from CLIENT for PROJECT-ROOT.
SESSION is the active session."
  (unwind-protect
      (rap--start-workspace session client project-root (rap--create-initialization-options session client))
    (rap--spinner-stop)))

(provide 'rap-client)
;;; rap-client.el ends here
