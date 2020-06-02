(require 'ewoc)

(defcustom rap-log-max 'all "Max log line count.")

(defmacro rap--json-serialize (params)
  (if (progn
        (require 'json)
        (fboundp 'json-serialize))
      `(json-serialize ,params
                       :null-object nil
                       :false-object :json-false)
    `(let ((json-false :json-false))
       (json-encode ,params))))

(defun rap--make-message (params)
  "Create a RAP message from PARAMS, after encoding it to a JSON string."
  (let ((body (rap--json-serialize params) ))
    (concat "Content-Length: "
            (number-to-string (1+ (string-bytes body)))
            "\r\n\r\n"
            body
            "\n")))

(cl-defstruct rap--log-entry timestamp process-time type method id body)

(defun rap--make-log-entry (method id body type &optional process-time)
  "Create an outgoing log object from BODY with method METHOD and id ID.
If ID is non-nil, then the body is assumed to be a notification.
TYPE can either be 'incoming or 'outgoing"
  (cl-assert (memq type '(incoming-req outgoing-req incoming-notif
                                       outgoing-notif incoming-resp
                                       outgoing-resp)))
  (make-rap--log-entry
   :timestamp (format-time-string "%I:%M:%S %p")
   :process-time process-time
   :method method
   :id id
   :type type
   :body body))

(defun rap--log-entry-pp (entry)
  (cl-assert (rap--log-entry-p entry))
  (pcase-let (((cl-struct rap--log-entry timestamp method id type process-time
                          body)
               entry)
              (json-false :json-false)
              (json-encoding-pretty-print t)
              (str nil))
    (setq str
          (concat (format "[Trace - %s] " timestamp)
                  (pcase type
                    ('incoming-req (format "Received request '%s - (%s)." method id))
                    ('outgoing-req (format "Sending request '%s - (%s)'." method id))

                    ('incoming-notif (format "Received notification '%s'." method))
                    ('outgoing-notif (format "Sending notification '%s'." method))

                    ('incoming-resp (format "Received response '%s - (%s)' in %dms."
                                            method id process-time))
                    ('outgoing-resp
                     (format
                      "Sending response '%s - (%s)'. Processing request took %dms"
                      method id process-time)))
                  "\n"
                  (if (memq type '(incoming-resp ougoing-resp))
                      "Result: "
                    "Params: ")
                  (json-encode body)
                  "\n\n\n"))
    (setq str (propertize str 'mouse-face 'highlight 'read-only t))
    (insert str)))

;; rap-log-io-mode

;; Buffer local variable for storing number of lines.
(defvar rap--log-lines)

(defvar rap-log-io-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'rap-log-io-next)
    (define-key map (kbd "M-p") #'rap-log-io-prev)
    (define-key map (kbd "k") #'rap--erase-log-buffer)
    (define-key map (kbd "K") #'rap--erase-session-log-buffers)
    map)
  "Keymap for rap log buffer mode.")

(define-derived-mode rap-log-io-mode special-mode "RapLogIo"
  "Special mode for viewing IO logs.")

(defun rap-workspace-show-log (workspace)
  "Display the log buffer of WORKSPACE."
  (interactive
   (list (if rap-print-io
             (if (eq (length (rap-workspaces)) 1)
                 (cl-first (rap-workspaces))
               (rap--completing-read "Workspace: " (rap-workspaces)
                                     #'rap--workspace-print nil t))
           (user-error "IO logging is disabled"))))
  (switch-to-buffer (rap--get-log-buffer-create workspace)))

(defalias 'rap-switch-to-io-log-buffer 'rap-workspace-show-log)

(defun rap--get-log-buffer-create (workspace)
  "Return the rap log buffer of WORKSPACE, creating a new one if needed."
  (let ((server-id (-> workspace rap--workspace-client rap--client-server-id symbol-name))
        (pid (format "%s" (process-id (rap--workspace-cmd-proc workspace)))))
    (get-buffer-create (format "*rap-log: %s:%s*" server-id pid))))

(defun rap--erase-log-buffer (&optional all)
  "Delete contents of current rap log buffer.
When ALL is t, erase all log buffers of the running session."
  (interactive)
  (let* ((workspaces (rap--session-workspaces (rap-session)))
         (current-log-buffer (current-buffer)))
    (dolist (w workspaces)
      (let ((b (rap--get-log-buffer-create w)))
        (when (or all (eq b current-log-buffer))
          (with-current-buffer b
            (let ((inhibit-read-only t))
              (erase-buffer))))))))

(defun rap--erase-session-log-buffers ()
  "Erase log buffers of the running session."
  (interactive)
  (rap--erase-log-buffer t))

(defun rap-log-io-next (arg)
  "Move to next log entry."
  (interactive "P")
  (ewoc-goto-next rap--log-io-ewoc (or arg 1)))

(defun rap-log-io-prev (arg)
  "Move to previous log entry."
  (interactive "P")
  (ewoc-goto-prev rap--log-io-ewoc (or arg 1)))
(defvar-local rap--log-io-ewoc nil)

(defun rap--get-create-io-ewoc (workspace)
  (if (and (rap--workspace-ewoc workspace)
           (buffer-live-p (ewoc-buffer (rap--workspace-ewoc workspace))))
      (rap--workspace-ewoc workspace)
    (with-current-buffer (rap--get-log-buffer-create workspace)
      (unless (eq 'rap-log-io-mode major-mode) (rap-log-io-mode))
      (setq-local window-point-insertion-type t)
      (setq rap--log-io-ewoc (ewoc-create #'rap--log-entry-pp nil nil t))
      (setf (rap--workspace-ewoc workspace) rap--log-io-ewoc))
    (rap--workspace-ewoc workspace)))

(defun rap--log-entry-new (entry workspace)
  (let* ((ewoc (rap--get-create-io-ewoc workspace))
         (count (and (not (eq rap-io-messages-max t)) (rap--ewoc-count ewoc)))
         (node (if (or (eq rap-io-messages-max t)
                       (>= rap-io-messages-max count))
                   nil
                 (ewoc-nth ewoc (1- rap-io-messages-max))))
         (prev nil)
         (inhibit-read-only t))
    (while node
      (setq prev (ewoc-prev ewoc node))
      (ewoc-delete ewoc node)
      (setq node prev))
    (ewoc-enter-last ewoc entry)))

(defun rap-log (format &rest args)
  "Log message to the ’*rap-log*’ buffer.

FORMAT and ARGS i the same as for `message'."
  (when rap-log-max
    (let ((log-buffer (get-buffer "*rap-log*"))
          (inhibit-read-only t))
      (unless log-buffer
        (setq log-buffer (get-buffer-create "*rap-log*"))
        (with-current-buffer log-buffer
          (view-mode 1)
          (set (make-local-variable 'rap--log-lines) 0)))
      (with-current-buffer log-buffer
        (save-excursion
          (let* ((message (apply 'format format args))
                 ;; Count newlines in message.
                 (newlines (1+ (cl-loop with start = 0
                                        for count from 0
                                        while (string-match "\n" message start)
                                        do (setq start (match-end 0))
                                        finally return count))))
            (goto-char (point-max))

            ;; in case the buffer is not empty insert before last \n to preserve
            ;; the point position(in case it is in the end)
            (if (eq (point) (point-min))
                (progn
                  (insert "\n")
                  (backward-char))
              (backward-char)
              (insert "\n"))
            (insert message)

            (setq rap--log-lines (+ rap--log-lines newlines))

            (when (and (integerp rap-log-max) (> rap--log-lines rap-log-max))
              (let ((to-delete (- rap--log-lines rap-log-max)))
                (goto-char (point-min))
                (forward-line to-delete)
                (delete-region (point-min) (point))
                (setq rap--log-lines rap-log-max)))))))))

(defun rap--info (format &rest args)
  "Display rap info message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "RAP" 'face 'success) (apply #'format format args)))

(defun rap--warn (format &rest args)
  "Display rap warn message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "RAP" 'face 'warning) (apply #'format format args)))

(defun rap--error (format &rest args)
  "Display rap error message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "RAP" 'face 'error) (apply #'format format args)))

(provide 'rap-log)
