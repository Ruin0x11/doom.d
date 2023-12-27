;;; site-lisp/lsp-roslyn.el -*- lexical-binding: t; -*-

(require 'lsp-mode)

(defun listen-filter (proc string)
  (message "%s" string))

(defun listen-sentinel (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (message (format "client %s has quit" proc))))

(defun keep-output (process output)
  (let* ((data (json-parse-string output :object-type 'plist))
         (pipe (plist-get data :pipeName)))
    (when pipe
      (string-match "\\([a-z0-9]+\\)$" pipe)
      (let ((pipe-name (match-string 1 pipe)))
        (message "%s %s" pipe pipe-name)
        (setq lsp-roslyn--pipe-name pipe-name)))))

(defvar lsp-roslyn--pipe-name nil)

(defun lsp-roslyn-make-connection (filter sentinel name environment-fn workspace)
  (setq lsp-roslyn--pipe-name nil)
  (let ((command-process (make-process
                          :name "dotnet"
                          :buffer "*dotnet*"
                          :coding 'no-conversion
                          :filter 'keep-output
                          :sentinel sentinel
                          :stderr "*dotnet-stderr*"
                          :command (list "dotnet"
                                         "G:\\build\\roslyn\\artifacts\\bin\\Microsoft.CodeAnalysis.LanguageServer\\Debug\\net7.0\\Microsoft.CodeAnalysis.LanguageServer.dll"
                                         "--logLevel=Information" "--extensionLogDirectory=c:\\temp\\")
                          :noquery t)))
    (accept-process-output command-process 5) ; wait for JSON with pipe name to print on stdout
    (let* ((process-environment
            (lsp--compute-process-environment environment-fn))
           (process-name (generate-new-buffer-name name))
           (stderr-buf (format "*%s::stderr*" process-name))
           (default-directory (lsp--default-directory-for-connection))
           ;; (communication-process (lsp-json-rpc-connection workspace (list "PowerShell" "-NoProfile" "-ExecutionPolicy" "Bypass" "-Command" "C:\\bin\\stdpipe.ps1" "." lsp-roslyn--pipe-name)))
           (communication-process (make-process
                                   :name process-name
                                   :connection-type 'pipe
                                   :buffer (format "*%s*" process-name)
                                   :coding 'no-conversion
                                   :filter filter
                                   :sentinel sentinel
                                   :stderr stderr-buf
                                   :noquery t
                                   :command (list "PowerShell" "-NoProfile" "-ExecutionPolicy" "Bypass" "-Command" "C:\\bin\\stdpipe.ps1" "." lsp-roslyn--pipe-name))))
      (message "go %s" lsp-roslyn--pipe-name)
      (with-current-buffer (get-buffer stderr-buf)
        ;; Make the *NAME::stderr* buffer buffer-read-only, q to bury, etc.
        (special-mode))
      (with-current-buffer (get-buffer "*dotnet-stderr*")
        (special-mode))
      (set-process-query-on-exit-flag command-process nil)
      (set-process-query-on-exit-flag communication-process nil)
      (set-process-query-on-exit-flag (get-buffer-process stderr-buf) nil)
      (cons communication-process communication-process))))

(defun lsp-roslyn-make-connection-1 (filter sentinel name environment-fn workspace)
  (setq lsp-roslyn--pipe-name "d1b72351")
  (let* ((process-environment
          (lsp--compute-process-environment environment-fn))
         (process-name (generate-new-buffer-name name))
         (stderr-buf (format "*%s::stderr*" process-name))
         (default-directory (lsp--default-directory-for-connection))
         ;; (communication-process (lsp-json-rpc-connection workspace (list "PowerShell" "-NoProfile" "-ExecutionPolicy" "Bypass" "-Command" "C:\\bin\\stdpipe.ps1" "." lsp-roslyn--pipe-name)))
         (communication-process (make-process
                                 :name process-name
                                 :connection-type 'pipe
                                 :buffer (format "*%s*" process-name)
                                 :coding 'no-conversion
                                 :filter filter
                                 :sentinel sentinel
                                 :stderr stderr-buf
                                 :noquery t
                                 :command (list "PowerShell" "-NoProfile" "-ExecutionPolicy" "Bypass" "-Command" "C:\\bin\\stdpipe.ps1" "." lsp-roslyn--pipe-name)))
         )
    (message "go %s" lsp-roslyn--pipe-name)
    (with-current-buffer (get-buffer stderr-buf)
      ;; Make the *NAME::stderr* buffer buffer-read-only, q to bury, etc.
      (special-mode))
    (set-process-query-on-exit-flag communication-process nil)
    (set-process-query-on-exit-flag (get-buffer-process stderr-buf) nil)
    (cons communication-process communication-process)))

(lsp-defun lsp-roslyn--log-message (workspace params)
  (message "%s: %s" (gethash "type" params) (gethash "message" params)))

(defun lsp-roslyn-open-solution ()
  (interactive)
  (lsp-notify "solution/open" (list :solution (lsp--path-to-uri "C:/Users/yuno/build/Scanner/Scanner.sln"))))

(defun lsp-roslyn--uri-to-path (uri)
  "Convert URI to a file path."
  (let* ((url (url-generic-parse-url uri))
         (type (url-type url))
         (target (url-target url))
         (file
          (concat (decode-coding-string (url-filename url)
                                        (or locale-coding-system 'utf-8))
                  (when (and target
                             (not (s-match
                                   (rx "#" (group (1+ num)) (or "," "#")
                                       (group (1+ num))
                                       string-end)
                                   uri)))
                    (concat "#" target))))
         (file-name (if (and type (not (string= type "file")))
                        (if-let ((handler (lsp--get-uri-handler type)))
                            (funcall handler uri)
                          uri)
                      ;; `url-generic-parse-url' is buggy on windows:
                      ;; https://github.com/emacs-lsp/lsp-mode/pull/265
                      (or (and (eq system-type 'windows-nt)
                               (eq (elt file 0) ?\/)
                               (substring file 1))
                          file))))
    (->> file-name
         (concat (-some #'lsp--workspace-host-root (lsp-workspaces)))
         (lsp-remap-path-if-needed))))

(defun lsp-roslyn--path-to-uri (path)
  (concat lsp--uri-file-prefix
          (--> path
               (expand-file-name it)
               (or (file-remote-p it 'localname t) it))))

(defun lsp-roslyn--before-file-open (_workspace)
  "Set `lsp-buffer-uri' variable after C# file is open from *.metadata-uri file."
  (lsp-request-async "textDocument/diagnostic"
                     (list :textDocument (lsp--text-document-identifier))
                     (lambda (data)
                       (message "%s" data))
                     :mode 'tick))

;; (lsp-request "textDocument/diagnostic" (list :textDocument (lsp--text-document-identifier)))


(lsp-register-client
 (make-lsp-client :new-connection '(:connect (lambda (f s n e w) (lsp-roslyn-make-connection f s n e w)) :test? (lambda () t))
                  :priority 999
                  :server-id 'vscode-csharp
                  :activation-fn (lsp-activate-on "csharp")
                  :notification-handlers (ht ("window/logMessage" 'lsp-roslyn--log-message))
                  ;; Parts of the Roslyn server do not strictly follow the LSP spec.
                  ;; These two functions are the same as lsp-mode's except they do not
                  ;; (un)hexify URIs.
                  :path->uri-fn 'lsp-roslyn--path-to-uri
                  :uri->path-fn 'lsp-roslyn--uri-to-path

                  :before-file-open-fn #'lsp-roslyn--before-file-open
                  ;; :uri-handlers (ht ("csharp" #'lsp-csharp--cls-metadata-uri-handler))
                  ;; :download-server-fn #'lsp-csharp--cls-download-server)
                  ))
