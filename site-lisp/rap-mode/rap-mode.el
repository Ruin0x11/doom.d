;;; rap-mode.el --- REPL Adapter Protocol mode      -*- lexical-binding: t; -*-

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
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "6.0") (dash-functional "1.2.0") (bui "1.1.0") (f "0.20.0") (s "1.12.0") (lsp-treemacs "0.1"))
;; Version: 0.3

;;; Commentary:
;; REPL Adapter Protocol client for Emacs.

;;; Code:

(require 'lsp)
(require 'json)
(require 'f)
(require 'dash)
(require 'cl-lib)
(require 'ansi-color)
(require 'rap-log)

(defcustom rap-history-file (expand-file-name (locate-user-emacs-file ".rap-history"))
  "Where to persist REPL history"
  :group 'rap-mode
  :type 'file)

(defcustom rap-auto-show-repl t
  "If non-nil, the REPL will be showen automatically."
  :group 'rap-mode
  :type 'boolean)

(defcustom rap-print-io t
  "If non-nil, print all messages to and from the RAP to messages."
  :group 'rap-mode
  :type 'boolean)

(defcustom rap-inhibit-io nil
  "If non-nil, the messages will be inhibited."
  :group 'rap-mode
  :type 'boolean)

(defcustom rap-label-output-buffer-category nil
  "If non-nil, content that is printed to the output buffer will be labelled based on RAP protocol category."
  :group 'rap-mode
  :type 'boolean)

(defcustom rap-output-window-min-height 10
  "The minimum height of the output window."
  :group 'rap-mode
  :type 'number)

(defcustom rap-output-window-max-height 20
  "The maximum height of the output window."
  :group 'rap-mode
  :type 'number)

(defcustom rap-terminated-hook nil
  "List of functions to be called after a debug session has been terminated.

The functions will received the debug dession that
has been terminated."
  :type 'hook
  :group 'rap-mode)

(defcustom rap-stopped-hook nil
  "List of functions to be called after a breakpoint has been hit."
  :type 'hook
  :group 'rap-mode)

(defcustom rap-session-changed-hook nil
  "List of functions to be called after sessions have changed."
  :type 'hook
  :group 'rap-mode)

(defcustom rap-executed-hook nil
  "List of functions that will be called after execution and processing request."
  :type 'hook
  :group 'rap-mode)

(defvar rap--repl-providers (make-hash-table :test 'equal))

(defcustom rap-debug-template-configurations nil
  "Plist Template configurations for DEBUG/RUN."
  :safe #'listp
  :group 'rap-mode
  :type '(plist))

(defvar rap--repl-configuration nil
  "List of the previous configuration that have been executed.")

(defvar rap-connect-retry-count 1
  "Retry count for RAP connect.")

(defvar rap-connect-retry-interval 0.02
  "Retry interval for RAP connect.")

(defun dash-expand:&rap-session (key source)
  `(,(intern-soft (format "rap--repl-session-%s" (eval key) )) ,source))

(cl-defstruct rap--repl-session
  (name nil)
  ;; ‘last-id’ is the last JSON-RPC identifier used.
  (last-id 0)
  (proc nil :read-only t)
  ;; ‘response-handlers’ is a hash table mapping integral JSON-RPC request
  ;; identifiers for pending asynchronous requests to functions handling the
  ;; respective responses.  Upon receiving a response from the language server,
  ;; ‘rap-mode’ will call the associated response handler function with a
  ;; single argument, the deserialized response parameters.
  (response-handlers (make-hash-table :test 'eql) :read-only t)
  ;; RAP parser.
  (parser (make-rap--parser) :read-only t)
  (output-buffer nil)
  (thread-id nil)
  ;; reference to the workspace that holds the information about the lsp workspace.
  (workspace nil)
  ;; one of 'pending 'running 'terminated 'failed
  (state 'pending)
  ;; the arguments that were used to start the debug session.
  (launch-args nil)
  ;; The result of initialize request. It holds the server capabilities.
  (initialize-result nil)
  (error-message nil))

(cl-defstruct rap--parser
  (waiting-for-response nil)
  (response-result nil)

  ;; alist of headers
  (headers '())

  ;; message body
  (body nil)

  ;; If non-nil, reading body
  (reading-body nil)

  ;; length of current message body
  (body-length nil)

  ;; amount of current message body currently stored in 'body'
  (body-received 0)

  ;; Leftover data from previous chunk; to be processed
  (leftovers nil))

(defun rap--get-sessions ()
  "Get sessions for WORKSPACE."
  (lsp-workspace-get-metadata "rap-sessions"))

(defun rap--cur-session ()
  "Get currently active `rap--repl-session'."
  (lsp-workspace-get-metadata "rap-default-session"))

(defun rap--resp-handler (&optional success-callback)
  "Generate response handler.

The handler will call `error' on failure.
SUCCESS-CALLBACK will be called if it is provided and if the call
has succeeded."
  (-lambda ((result &as &hash "success" "message"))
    (if success
        (when success-callback (funcall success-callback result))
      (error message))))

(defun rap--session-init-resp-handler (debug-session &optional success-callback)
  "Returned handler will mark the DEBUG-SESSION as failed if call return error.

SUCCESS-CALLBACK will be called if it is provided and if the call
has succeeded."
  (-lambda ((result &as &hash "success" "message"))
    (if success
        (when success-callback (funcall success-callback result))
      (warn "Initialize request failed: %s" message)
      (delete-process (rap--repl-session-proc debug-session))

      (setf (rap--repl-session-state debug-session) 'failed
            (rap--repl-session-error-message debug-session) message)

      (run-hook-with-args 'rap-terminated-hook debug-session)
      (run-hooks 'rap-session-changed-hook))))

(defun rap--cur-session-or-die ()
  "Get currently selection `rap--repl-session' or die."
  (or (rap--cur-session) (error "No active current session")))

(defun rap--session-running (debug-session)
  "Check whether DEBUG-SESSION still running."
  (and debug-session
       (not (memq (rap--repl-session-state debug-session) '(terminated failed)))))

(defun rap--cur-active-session-or-die ()
  "Get currently non-terminated  `rap--repl-session' or die."
  (-let ((debug-session (rap--cur-session-or-die)))
    (if (rap--session-running debug-session)
        debug-session
      (error "Session %s is terminated" (rap--repl-session-name debug-session)))))

(defun rap--set-cur-session (debug-session)
  "Change the active debug session to DEBUG-SESSION."
  (lsp-workspace-set-metadata "rap-default-session" debug-session))

(defmacro rap--put-if-absent (config key form)
  "Update KEY to FORM if KEY does not exist in plist CONFIG."
  `(plist-put ,config ,key (or (plist-get ,config ,key) ,form)))

(defun rap--completing-read (prompt collection transform-fn &optional predicate
                                    require-match initial-input
                                    hist def inherit-input-method)
  "Wrap `completing-read' to provide tranformation function.

TRANSFORM-FN will be used to transform each of the items before displaying.

PROMPT COLLECTION PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD will be proxied to `completing-read' without changes."
  (let* ((result (--map (cons (funcall transform-fn it) it) collection))
         (completion (completing-read prompt (-map 'cl-first result)
                                      predicate require-match initial-input hist
                                      def inherit-input-method)))
    (cdr (assoc completion result))))

(defun rap--plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (cl-first plist)))
          (setq p (plist-put p (cl-first plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun rap--json-encode (params)
  "Create a LSP message from PARAMS, after encoding it to a JSON string."
  (let* ((json-encoding-pretty-print rap-print-io)
         (json-false :json-false))
    (json-encode params)))

(defun rap--make-message (params)
  "Create a LSP message from PARAMS, after encoding it to a JSON string."
  (let* ((body (rap--json-encode params)))
    (format "Content-Length: %d\r\n\r\n%s" (string-bytes body) body)))

(defun rap--parse-header (s)
  "Parse string S as a RAP (KEY . VAL) header."
  (let ((pos (string-match "\:" s))
        key val)
    (unless pos
      (signal 'lsp-invalid-header-name (list s)))
    (setq key (substring s 0 pos)
          val (substring s (+ 2 pos)))
    (when (string-equal key "Content-Length")
      (cl-assert (cl-loop for c being the elements of val
                          when (or (> c ?9) (< c ?0)) return nil
                          finally return t)
                 nil (format "Invalid Content-Length value: %s" val)))
    (cons key val)))

(defun rap--persist (file-name to-persist)
  "Persist TO-PERSIST.

FILE-NAME the file name.
WORKSPACE will be used to calculate root folder."
  (with-demoted-errors
      "Failed to persist file: %S"
    (make-directory (file-name-directory file-name) t)
    (with-temp-file file-name
      (erase-buffer)
      (insert (prin1-to-string to-persist)))))

(defun rap--set-sessions (debug-sessions)
  "Update list of debug sessions for WORKSPACE to DEBUG-SESSIONS."
  (lsp-workspace-set-metadata "rap-sessions" debug-sessions)
  (run-hook-with-args 'rap-session-changed-hook))

; (defun rap--persist-breakpoints (breakpoints)
;   "Persist BREAKPOINTS."
;   ;; filter markers before persisting the breakpoints (markers are not
;   ;; writeable) and update the point based on the marker.
;   (-let [filtered-breakpoints (make-hash-table :test 'equal)]
;     (maphash (lambda (k v)
;                (puthash k (-map (-lambda ((bkp &as &plist :marker :point))
;                                   (-> bkp
;                                       (rap--plist-delete :point)
;                                       (rap--plist-delete :marker)
;                                       (plist-put :point (if marker
;                                                             (marker-position marker)
;                                                           point))))
;                                 v)
;                         filtered-breakpoints))
;              breakpoints)
;     (rap--persist rap-breakpoints-file filtered-breakpoints)))

(defun rap--get-body-length (headers)
  "Get body length from HEADERS."
  (let ((content-length (cdr (assoc "Content-Length" headers))))
    (if content-length
        (string-to-number content-length)

      ;; This usually means either the server our our parser is
      ;; screwed up with a previous Content-Length
      (error "No Content-Length header"))))

(defun rap--parser-reset (p)
  "Reset `rap--parser' P."
  (setf
   (rap--parser-leftovers p) ""
   (rap--parser-body-length p) nil
   (rap--parser-body-received p) nil
   (rap--parser-headers p) '()
   (rap--parser-body p) nil
   (rap--parser-reading-body p) nil))

(defun rap--parser-read (p output)
  "Parser OUTPUT using parser P."
  (let* ((messages '())
         (output (string-as-unibyte output))
         (chunk (concat (rap--parser-leftovers p) output)))
    (while (not (string-empty-p chunk))
      (if (not (rap--parser-reading-body p))
          ;; Read headers
          (let* ((body-sep-pos (string-match-p "\r\n\r\n" chunk)))
            (if body-sep-pos
                ;; We've got all the headers, handle them all at once:
                (let* ((header-raw (substring chunk 0 body-sep-pos))
                       (content (substring chunk (+ body-sep-pos 4)))
                       (headers
                        (mapcar 'rap--parse-header
                                (split-string header-raw "\r\n")))
                       (body-length (rap--get-body-length headers)))
                  (setf
                   (rap--parser-headers p) headers
                   (rap--parser-reading-body p) t
                   (rap--parser-body-length p) body-length
                   (rap--parser-body-received p) 0
                   (rap--parser-body p) (make-string body-length ?\0)
                   (rap--parser-leftovers p) nil)
                  (setq chunk content))

              ;; Haven't found the end of the headers yet. Save everything
              ;; for when the next chunk arrives and await further input.
              (setf (rap--parser-leftovers p) chunk)
              (setq chunk "")))

        ;; Read body
        (let* ((total-body-length (rap--parser-body-length p))
               (received-body-length (rap--parser-body-received p))
               (chunk-length (string-bytes chunk))
               (left-to-receive (- total-body-length received-body-length))
               (this-body (substring chunk 0 (min left-to-receive chunk-length)))
               (leftovers (substring chunk (string-bytes this-body))))
          (store-substring (rap--parser-body p) received-body-length this-body)
          (setf (rap--parser-body-received p) (+ (rap--parser-body-received p)
                                                 (string-bytes this-body)))
          (when (>= chunk-length left-to-receive)
            (push (decode-coding-string (rap--parser-body p) 'utf-8) messages)
            (rap--parser-reset p))

          (setq chunk leftovers))))
    (nreverse messages)))

(defun rap--read-json (str)
  "Read the JSON object contained in STR and return it."
  (let* ((json-array-type 'list)
         (json-object-type 'hash-table)
         (json-false nil))
    (json-read-from-string str)))

(defun rap--buffer-list ()
  "Get all file backed buffers."
  (-filter 'buffer-file-name (buffer-list)))

(defun rap--mark-session-as-terminated (debug-session)
  "Mark DEBUG-SESSION as terminated."
  (setf (rap--repl-session-state debug-session) 'terminated)

  (with-demoted-errors "Process cleanup failed with %s"
    (delete-process (rap--repl-session-proc debug-session)))

  (run-hook-with-args 'rap-terminated-hook debug-session))

(defun rap--output-buffer-format-with-category (category output)
  "Formats a string suitable for printing to the output buffer using CATEGORY and OUTPUT."
  (let ((message (format "%s: %s" category output)))
    (if (string= (substring message -1) "\n")
        message
      (concat message "\n"))))

(defun rap--output-buffer-format (output-body)
  "Formats a string suitable for printing to the output buffer using an OUTPUT-BODY."
  (if rap-label-output-buffer-category
      (rap--output-buffer-format-with-category (gethash "category" output-body)
                                               (gethash "output" output-body))
    (gethash "output" output-body)))

(defun rap--insert-at-point-max (str)
  "Inserts STR at point-max of the buffer."
  (goto-char (point-max))
  (insert (ansi-color-apply str)))

(defun rap--print-to-output-buffer (debug-session str)
  "Insert content from STR into the output buffer associated with DEBUG-SESSION."
  (with-current-buffer (get-buffer-create (rap--repl-session-output-buffer debug-session))
    (font-lock-mode t)
    (setq-local buffer-read-only nil)
    (if (and (eq (current-buffer) (window-buffer (selected-window)))
             (not (= (point) (point-max))))
        (save-excursion
          (rap--insert-at-point-max str))
      (rap--insert-at-point-max str))
    (setq-local buffer-read-only t)))

(defun rap--on-event (debug-session event)
  "Dispatch EVENT for DEBUG-SESSION."
  (-let [(&hash "body" "event" event-type) event]
    (pcase event-type
      (_ (rap--error "No message handler for %s" event-type)))))

(defun rap--create-filter-function (debug-session)
  "Create filter function for DEBUG-SESSION."
  (let ((parser (rap--repl-session-parser debug-session))
        (handlers (rap--repl-session-response-handlers debug-session)))
    (lambda (_ msg)
      (mapc (lambda (m)
              (let* ((parsed-msg (rap--read-json m))
                     (key (gethash "request_seq" parsed-msg nil)))
                (when rap-print-io
                  (let ((inhibit-message rap-inhibit-io))
                    (rap-log "Received:\n%s" (rap--json-encode parsed-msg))))
                (pcase (gethash "type" parsed-msg)
                  ("event" (rap--on-event debug-session parsed-msg))
                  ("response" (if-let (callback (gethash key handlers nil))
                                  (progn
                                    (funcall callback parsed-msg)
                                    (remhash key handlers)
                                    (run-hook-with-args 'rap-executed-hook
                                                        debug-session
                                                        (gethash "command" parsed-msg)))
                                (rap--error "Unable to find handler for %s." (pp parsed-msg))))
                  ("request" (-let* (((&hash "arguments" (&hash? "args" "cwd" "title") "command" "seq") parsed-msg)
                                     (default-directory cwd))
                               (async-shell-command (s-join " " args))
                               (rap--send-message (rap--make-response seq)
                                                  (rap--resp-handler)
                                                  debug-session))))))
            (rap--parser-read parser msg)))))

(defun rap--create-output-buffer (session-name)
  "Creates an output buffer with with name SESSION-NAME."
  (with-current-buffer (get-buffer-create (concat "*" session-name " out*"))
    (set (make-local-variable 'window-point-insertion-type) t)
    (current-buffer)))

(defun rap--make-request (command &optional args)
  "Make request for COMMAND with arguments ARGS."
  (if args
      (list :command command
            :arguments args
            :type "request")
    (list :command command
          :type "request")))

(defun rap--make-response (id &optional args)
  "Make request for COMMAND with arguments ARGS."
  (list :request_seq id
        :success t
        :type "response"))

(defun rap--initialize-message (arapter-id)
  "Create initialize message.
ARAPTER-ID the id of the arapter."
  (list :command "initialize"
        :arguments (list :clientID "vscode"
                         :clientName "Visual Studio Code"
                         :arapterID arapter-id
                         :pathFormat "path"
                         :linesStartAt1 t
                         :columnsStartAt1 t
                         :supportsVariableType t
                         :supportsVariablePaging t
                         :supportsRunInTerminalRequest t
                         :locale "en-us")
        :type "request"))

(defun rap--json-pretty-print (msg)
  "Convert json MSG string to pretty printed json string."
  (let ((json-encoding-pretty-print t))
    (json-encode (json-read-from-string msg))))

(defun rap--send-message (message callback debug-session)
  "MESSAGE DEBUG-SESSION CALLBACK."
  (if (rap--session-running debug-session)
      (let* ((request-id (cl-incf (rap--repl-session-last-id debug-session)))
             (message (plist-put message :seq request-id)))
        (puthash request-id callback (rap--repl-session-response-handlers debug-session))
        (when rap-print-io
          (let ((inhibit-message rap-inhibit-io))
            (rap-log "Sending: \n%s" (rap--json-encode message))))
        (process-send-string (rap--repl-session-proc debug-session)
                             (rap--make-message message)))
    (error "Session %s is already terminated" (rap--repl-session-name debug-session))))

(defun rap-request (session method &rest args)
  "Sync request."
  (let (result)
    (rap--send-message
     (rap--make-request method args)
     (lambda (res) (setf result (or res :finished)))
     session)

    (while (not result)
      (accept-process-output nil 0.001))

    (cond
     ((eq result :finished) nil)
     ((and (ht? result) (not (gethash "success" result))) (error (gethash "message" result)))
     (t (gethash "body" result)))))

(defun rap--open-network-stream (session-name host port)
  (let ((retries 0)
        result)
    (while (and (not result)
                (< retries rap-connect-retry-count))
      (condition-case err
          (setq result (open-network-stream session-name nil
                                            host port :type 'plain))
        (file-error
         (let ((inhibit-message rap-inhibit-io))
           (message "Failed to connect to %s:%s with error message %s"
                    host
                    port
                    (error-message-string err))
           (sit-for rap-connect-retry-interval)
           (setq retries (1+ retries))))))
    (or result (error "Failed to connect to port %s" port))))

(defun rap--create-session (launch-args)
  "Create debug session from LAUNCH-ARGS."
  (-let* (((&plist :host :name session-name :port port) launch-args)
          (proc (rap--open-network-stream session-name host port))
          (debug-session (make-rap--repl-session
                          :launch-args launch-args
                          :proc proc
                          :name session-name
                          :output-buffer (rap--create-output-buffer session-name))))
    (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
    (set-process-sentinel proc
                          (lambda (_process exit-str)
                            (message "REPL session process exited with status: %s" exit-str)
                            (rap--mark-session-as-terminated debug-session)))
    (set-process-filter proc (rap--create-filter-function debug-session))
    debug-session))

(defun rap--send-configuration-done (debug-session)
  "Send 'configurationDone' message for DEBUG-SESSION."
  (rap--send-message (rap--make-request "configurationDone")
                     (rap--resp-handler
                      (lambda (_)
                        (when (eq 'pending (rap--repl-session-state debug-session))
                          (setf (rap--repl-session-state debug-session) 'running)
                          (run-hook-with-args 'rap-session-changed-hook))))
                     debug-session))

(defun rap-eval (expression)
  "Eval and print EXPRESSION."
  (interactive "sEval: ")
  (let ((repl-session (rap--cur-active-session-or-die)))
    (rap--send-message
     (rap--make-request "evaluate"
                        (list :expression expression))
     (-lambda ((&hash "success" "message" "body"))
       (rap-overlays--display-interactive-eval-result
        (if success (gethash "result" body) message)
        (point)))
     repl-session)))

(defun rap-eval-thing-at-point ()
  "Eval and print EXPRESSION."
  (interactive)
  (rap-eval (thing-at-point 'symbol)))

(defun rap-eval-region (start end)
  "Evaluate the region between START and END."
  (interactive "r")
  (rap-eval (buffer-substring-no-properties start end)))

(defun rap--calculate-unique-name (debug-session-name debug-sessions)
  "Calculate unique name with prefix DEBUG-SESSION-NAME.
DEBUG-SESSIONS - list of the currently active sessions."
  (let* ((base-name (or (cl-second (s-match "\\(.*\\)<.*>" debug-session-name))
                        debug-session-name))
         (counter 1)
         (session-name base-name))
    (while (--first (string= session-name (rap--repl-session-name it)) debug-sessions)
      (setq session-name (format "%s<%s>" base-name counter))
      (setq counter (1+ counter)))
    session-name))

(define-derived-mode rap-server-log-mode compilation-mode "REPL Adapter"
  (read-only-mode 1)
  (setq-local window-point-insertion-type t)
  ;; we need to move window point to the end of the buffer once because
  ;; `compilation-start' inserts initial message before displaying the buffer.
  (run-with-idle-timer 0 nil
                       (lambda (buf)
                         (with-current-buffer buf
                           (mapc (lambda (w)
                                   (set-window-point w (point-max)))
                                 (get-buffer-window-list))))
                       (current-buffer)))

(defun rap-start-debugging (launch-args)
  "Start debug session with LAUNCH-ARGS.
Special arguments:

:wait-for-port - boolean defines whether the debug configuration
should be started after the :port argument is taken.

:program-to-start - when set it will be started using `compilation-start'
before starting the debug process."
  (-let* (((&plist :name :skip-debug-session :cwd :program-to-start
                   :wait-for-port :type :request :port
                   :environment-variables :hostName host) launch-args)
          (session-name (rap--calculate-unique-name name (rap--get-sessions)))
          (default-directory (or cwd default-directory)))
    (mapc (-lambda ((env . value)) (setenv env value)) environment-variables)
    (plist-put launch-args :name session-name)

    (when program-to-start
      (compilation-start program-to-start 'rap-server-log-mode
                         (lambda (_) (concat "*" session-name " server log*"))))
    (when wait-for-port
      (rap--wait-for-port host port rap-connect-retry-count rap-connect-retry-interval))

    (unless skip-debug-session
      (let ((debug-session (rap--create-session launch-args)))
        (rap--send-message
         (rap--initialize-message type)
         (rap--session-init-resp-handler
          debug-session
          (lambda (initialize-result)
            (rap-log "Connected")
            (-let [debug-sessions (rap--get-sessions)]

              (setf (rap--repl-session-initialize-result debug-session) initialize-result)

              (rap--set-sessions (cons debug-session debug-sessions)))
            (when request
              (rap--send-message
               (rap--make-request request launch-args)
               (rap--session-init-resp-handler debug-session)
               debug-session))))
         debug-session)

        (rap--set-cur-session debug-session)
        (push (cons session-name launch-args) rap--repl-configuration)
        (run-hook-with-args 'rap-session-created-hook debug-session))
      (when (and program-to-start rap-auto-show-repl)
        (save-excursion (rap-go-to-output-buffer))))))

(defun rap--read-from-file (file)
  "Read FILE content."
  (with-temp-buffer
    (insert-file-contents file)
    (cl-first (read-from-string
               (buffer-substring-no-properties (point-min) (point-max))))))

(defun rap--after-initialize ()
  "After initialize handler."
  ;; (with-demoted-errors
  ;;     "Failed to load breakpoints for the current workspace with error: %S"
  ;;   (let ((breakpoints-file rap-breakpoints-file))
  ;;     (when (f-exists? breakpoints-file)
  ;;       (-let [breakpoints (rap--read-from-file breakpoints-file)]
  ;;         (maphash (lambda (file file-breakpoints)
  ;;                    (rap--set-breakpoints-in-file file file-breakpoints))
  ;;                  breakpoints)
  ;;         (lsp-workspace-set-metadata "Breakpoints" breakpoints)))))
  )

(defun rap-mode-line ()
  "Calculate RAP modeline."
  (when lsp-mode
    (-when-let (debug-session (rap--cur-session))
      (format "%s - %s|"
              (rap--repl-session-name debug-session)
              (rap--repl-session-state debug-session)))))

(defun rap--switch-to-session (new-session)
  "Make NEW-SESSION the active debug session."
  (rap--set-cur-session new-session)

  (run-hook-with-args 'rap-session-changed-hook lsp--cur-workspace))

(defun rap-switch-session ()
  "Switch current session interactively."
  (interactive)
  (lsp--cur-workspace-check)
  (pcase (reverse
          (--remove
           (or (not (rap--session-running it))
               (eq it (rap--cur-session)))
           (rap--get-sessions)))
    ('() (error "No active session to switch to"))
    (`(,debug-session) (rap--switch-to-session debug-session))
    (target-debug-sessions (rap--switch-to-session
                            (rap--completing-read "Select session: "
                                                  target-debug-sessions
                                                  #'rap--repl-session-name)))))

(defun rap-register-repl-provider (language-id provide-configuration-fn)
  "Register repl configuration provider for LANGUAGE-ID.

PROVIDE-CONFIGURATION-FN is a function which will be called when
function `rap-mode' has received a request to start debug session which
has language id = LANGUAGE-ID. The function must return debug
arguments which contain the debug port to use for opening TCP connection."
  (puthash language-id provide-configuration-fn rap--repl-providers))

(defun rap-register-debug-template (configuration-name configuration-settings)
  "Register configuration template CONFIGURATION-NAME.
CONFIGURATION-SETTINGS - plist containing the preset settings for the configuration."
  (setq rap-debug-template-configurations
        (delq (assoc configuration-name rap-debug-template-configurations)
              rap-debug-template-configurations))
  (add-to-list
   'rap-debug-template-configurations
   (cons configuration-name configuration-settings)))

(defun rap--find-available-port ()
  "Find available port on HOST starting from STARTING-PORT."
  (let ((process (make-network-process :name " *rap test-connection*"
                                       :family 'ipv4
                                       :service 0
                                       :server 't)))
    (prog1 (process-contact process :service)
      (delete-process process))))

(defun rap--wait-for-port (host port &optional retry-count sleep-interval)
  "Wait for PORT to be open on HOST.

RETRY-COUNT is the number of the retries.
SLEEP-INTERVAL is the sleep interval between each retry."
  (let ((success nil)
        (retries 0))
    (while (and (not success) (< retries (or retry-count 100)))
      (condition-case err
          (progn
            (delete-process (open-network-stream "*connection-test*" nil host port :type 'plain))
            (setq success t))
        (file-error
         (let ((inhibit-message t))
           (rap--error "Failed to connect to %s:%s with error message %s"
                    host
                    port
                    (error-message-string err))
           (sit-for (or sleep-interval 0.02))
           (setq retries (1+ retries))))))
    success))

(defun rap-debug-last ()
  "Debug last configuration."
  (interactive)
  (if-let (configuration (cdr (cl-first rap--repl-configuration)))
      (rap-debug configuration)
    (call-interactively 'rap-debug)))

(defun rap-debug-recent ()
  "Debug last configuration."
  (interactive)
  (->> (rap--completing-read "Select configuration: "
                             rap--repl-configuration
                             'cl-first nil t)
       cl-rest
       rap-debug))


(defun rap-debug (debug-args)
  "Run debug configuration DEBUG-ARGS.

If DEBUG-ARGS is not specified the configuration is generated
after selecting configuration template."
  (interactive (list (-> (rap--completing-read "Select configuration template: "
                                               rap-debug-template-configurations
                                               'cl-first nil t)
                         cl-rest
                         copy-tree)))
  (rap-start-debugging (or (-some-> (plist-get debug-args :type)
                             (gethash rap--repl-providers)
                             (funcall debug-args))
                           (user-error "Have you loaded the `%s' specific rap package?"
                                       (or (plist-get debug-args :type)
                                           (user-error "%s does not specify :type" debug-args))))))

(defun rap-go-to-output-buffer ()
  "Go to output buffer."
  (interactive)
  (let ((win (display-buffer-in-side-window
              (rap--repl-session-output-buffer (rap--cur-session-or-die))
              `((side . bottom) (slot . 5) (window-width . 0.20)))))
    (set-window-dedicated-p win t)
    (select-window win)
    (fit-window-to-buffer nil rap-output-window-max-height rap-output-window-min-height)))

(defun rap-delete-session (debug-session)
  "Remove DEBUG-SESSION.
If the current session it will be terminated."
  (interactive (list (rap--cur-session-or-die)))
  (let* ((cleanup-fn (lambda ()
                       (->> (rap--get-sessions)
                            (-remove-item debug-session)
                            (rap--set-sessions))
                       (when (eq (rap--cur-session) debug-session)
                         (rap--switch-to-session nil))
                       (-when-let (buffer (rap--repl-session-output-buffer debug-session))
                         (kill-buffer buffer)))))
    (if (not (rap--session-running debug-session))
        (funcall cleanup-fn)
      (rap--send-message (rap--make-request "disconnect"
                                            (list :restart :json-false))
                         (rap--resp-handler
                          (lambda (_resp) (funcall cleanup-fn)))
                         debug-session))))

(defun rap-delete-all-sessions ()
  "Terminate/remove all sessions."
  (interactive)
  (--each (rap--get-sessions)
    (when (rap--session-running it)
      (condition-case _err
          (rap--send-message (rap--make-request "disconnect"
                                                (list :restart :json-false))
                             (rap--resp-handler)
                             it)
        (error))))

  (rap--set-sessions ())
  (rap--switch-to-session nil))

(defun rap--buffer-killed ()
  "Buffer killed handler."
  )

(defun rap--after-open ()
  "Handler of after open hook."
  (when (buffer-file-name)
    (add-hook 'kill-buffer-hook 'rap--buffer-killed nil t)))

(defvar rap-mode-map
  (let ((rap-mode-map (make-sparse-keymap)))
    rap-mode-map)
  "Keymap for `rap-mode'.")

;;;###autoload
(define-minor-mode rap-mode
  "Global minor mode for RAP mode."
  :init-value nil
  :group 'rap-mode
  :global t
  :require 'rap-mode
  :lighter (:eval (rap-mode-line))
  (rap--after-initialize)
  (add-hook 'lsp-after-open-hook 'rap--after-open))

(defun rap-turn-on-rap-mode ()
  "Turn on function `rap-mode'."
  (interactive)
  (rap-mode t))

(provide 'rap-mode)
;;; rap-mode.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
