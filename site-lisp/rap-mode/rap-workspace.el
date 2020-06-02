;; Clean up the entire state of rap mode when Emacs is killed, to get rid of any
;; pending language servers.
(add-hook 'kill-emacs-hook #'rap--global-teardown)

(cl-defstruct rap---workspace
  (ewoc nil)
  (root nil)
  (client nil)
  (proc nil)
  )

(cl-defstruct rap-session
  ;; ‘metadata’ is a generic storage for workspace specific data. It is
  ;; accessed via `rap-workspace-set-metadata' and `rap-workspace-set-metadata'
  (metadata (make-hash-table :test 'equal)))

(defvar rap--session nil
  "Contain the `rap-session' for the current Emacs instance.")

(defun rap-session ()
  "Get the session associated with the current buffer."
  (or rap--session (make-rap-session)))

(defun rap--global-teardown ()
  "Unload working workspaces."
  (rap-foreach-workspace (rap--shutdown-workspace)))

(defun rap--shutdown-workspace (&optional restart)
  "Shut down the language server process for ‘rap--cur-workspace’."
  (with-demoted-errors "RAP error: %S"
    (let ((rap-response-timeout 0.5))
      (condition-case _err
          (rap-request "shutdown" (make-hash-table))
        (error (rap--error "Timeout while sending shutdown request."))))
    (rap-notify "exit" nil))
  (setf (rap--workspace-shutdown-action rap--cur-workspace) (or (and restart 'restart) 'shutdown))
  (rap--uninitialize-workspace))

(defun rap--uninitialize-workspace ()
  "Cleanup buffer state.
When a workspace is shut down, by request or from just
disappearing, unset all the variables related to it."
  (let ((proc (rap--workspace-cmd-proc rap--cur-workspace)))
    (when (process-live-p proc)
      (kill-process proc))
    (unless rap--buffer-workspaces
      (rap-managed-mode -1))))

(defmacro with-rap-workspace (workspace &rest body)
  "Helper macro for invoking BODY in WORKSPACE context."
  (declare (debug (form body))
           (indent 1))
  `(let ((rap--cur-workspace ,workspace)) ,@body))

(defmacro with-rap-workspaces (workspaces &rest body)
  "Helper macro for invoking BODY against multiple WORKSPACES."
  (declare (debug (form body))
           (indent 1))
  `(let ((rap--buffer-workspaces ,workspaces)) ,@body))

(defmacro rap-foreach-workspace (&rest body)
  "Execute BODY for each of the current workspaces."
  (declare (debug (form body)))
  `(--map (with-rap-workspace it ,@body) (rap-workspaces)))

(defmacro when-rap-workspace (workspace &rest body)
  "Helper macro for invoking BODY in WORKSPACE context if present."
  (declare (debug (form body))
           (indent 1))
  `(when-let (rap--cur-workspace ,workspace) ,@body))

(defun rap--update-key (table key fn)
  "Apply FN on value corresponding to KEY in TABLE."
  (let ((existing-value (gethash key table)))
    (if-let (new-value (funcall fn existing-value))
        (puthash key new-value table)
      (remhash key table))))

(defun rap--process-sentinel (workspace process exit-str)
  "Create the sentinel for WORKSPACE."
  (unless (process-live-p process)
    (let* ((status (process-status process))
           (stderr (-> workspace rap--workspace-proc process-name get-buffer)))

      (rap--warn "%s has exited (%s)"
                 (process-name (rap--workspace-proc workspace))
                 (string-trim-right exit-str))

      (rap--spinner-stop)
      (run-hook-with-args 'rap-after-uninitialized-functions workspace)
      (rap--info "Workspace %s shutdown." (rap--workspace-print workspace)))))

;; `file-local-name' was added in Emacs 26.1.
(defalias 'rap-file-local-name
  (if (fboundp 'file-local-name)
      'file-local-name
    (lambda (file)
      "Return the local name component of FILE."
      (or (file-remote-p file 'localname) file))))

(defun rap--start-workspace (session client-template root &optional initialization-options)
  "Create new workspace for CLIENT-TEMPLATE with project root ROOT.
INITIALIZATION-OPTIONS are passed to initialize function.
SESSION is the active session."
  (rap--spinner-start)
  (-let* ((default-directory root)
          (client (copy-rap--client client-template))
          (workspace (make-rap--workspace
                      :root root
                      :client client
                      :status 'starting
                      :buffers (list (current-buffer))
                      :host-root (file-remote-p root)))
          (server-id (rap--client-server-id client))
          (environment-fn (rap--client-environment-fn client))
          ((proc . cmd-proc) (funcall
                              (or (plist-get (rap--client-new-connection client) :connect)
                                  (user-error "Client %s is configured incorrectly" client))
                              (rap--create-filter-function workspace)
                              (apply-partially #'rap--process-sentinel workspace)
                              (format "%s" server-id)
                              environment-fn))
          (workspace-folders (gethash server-id (rap-session-server-id->folders session))))
    (setf (rap--workspace-proc workspace) proc
          (rap--workspace-cmd-proc workspace) cmd-proc)

    ;; update (rap-session-folder->servers) depending on whether we are starting
    ;; multi/single folder workspace
    (-map (lambda (project-root)
            (->> session
                 rap-session-folder->servers
                 (gethash project-root)
                 (cl-pushnew workspace)))
          (or workspace-folders (list root)))

    (with-rap-workspace workspace
      (run-hooks 'rap-before-initialize-hook)
      (rap-request-async
       "initialize"
       (append
        (list :processId nil
              :rootPath (rap-file-local-name (expand-file-name root))
              :clientInfo (list :name "emacs"
                                :version (emacs-version))
              :rootUri (rap--path-to-uri root)
              :capabilities (rap--client-capabilities
                             (rap--client-custom-capabilities client))
              :initializationOptions initialization-options
              :workDoneToken "1"))
       (lambda (response)
         (unless response
           (rap--spinner-stop)
           (signal 'rap-empty-response-error (list "initialize")))

         (setf (rap--workspace-server-capabilities workspace) (gethash "capabilities" response)
               (rap--workspace-status workspace) 'initialized)

         (with-rap-workspace workspace
           (rap-notify "initialized" (make-hash-table)))

         (with-rap-workspace workspace
           (run-hooks 'rap-after-initialize-hook))
         (rap--info "%s initialized successfully" (rap--workspace-print workspace)))
       :mode 'detached))
    workspace))

(defun rap--spinner-start ()
  "Start spinner indication."
  (condition-case _err (spinner-start 'progress-bar-filled) (error)))

(defun rap--spinner-stop ()
  "Stop the spinner in case all of the workspaces are started."
  (when (--all? (eq (rap--workspace-status it) 'initialized)
                rap--buffer-workspaces)
    (spinner-stop)))

(defun lsp--propertize (str type)
  "Propertize STR as per TYPE."
  (propertize str 'face (alist-get type lsp--message-type-face)))

(defun lsp-workspaces ()
  "Return the lsp workspaces associated with the current project."
  (if lsp--cur-workspace (list lsp--cur-workspace) lsp--buffer-workspaces))

(defun lsp--completing-read (prompt collection transform-fn &optional predicate
                                    require-match initial-input
                                    hist def inherit-input-method)
  "Wrap `completing-read' to provide transformation function.

TRANSFORM-FN will be used to transform each of the items before displaying.

PROMPT COLLECTION PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD will be proxied to `completing-read' without changes."
  (let* ((result (--map (cons (funcall transform-fn it) it) collection))
         (completion (completing-read prompt (-map 'cl-first result)
                                      predicate require-match initial-input hist
                                      def inherit-input-method)))
    (cdr (assoc completion result))))

(provide 'rap-workspace)
