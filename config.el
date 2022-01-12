;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ruin0x11"
      user-mail-address "ipickering2@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "MS Gothic" :size 14))

(defun ruin/init-cjk-font ()
  (interactive)
  (when (window-system) 
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset doom-font))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (ruin/init-cjk-font)))
  (ruin/init-cjk-font))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
; (load-theme 'doom-tomorrow-night t)
(setq doom-theme 'vscode-dark-plus)
(set-face-background 'mode-line "#454545")
(set-face-background 'mode-line-inactive "#303030")


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;;

(when (string= system-type "windows-nt")
  (set-clipboard-coding-system 'utf-16-le))

(setq tags-add-tables t)
(savehist-mode 1)

(require 'facemenu)

(after! recentf
  (recentf-load-list)
  (run-at-time nil (* 120 60) #'recentf-save-list)
  (add-hook 'find-file-hook #'recentf-save-list)) ; every 120 mins

(defun reload-site-lisp ()
  "Puts site-lisp and its subdirectories into load-path."
  (interactive)
  (when (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((dir (doom-path doom-private-dir "site-lisp"))
           (default-directory dir))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)
        (normal-top-level-add-subdirs-to-load-path)))))

(reload-site-lisp)

(after! evil
  (setq evil-ex-substitute-global nil
        scroll-margin 5)
  (map! :leader
        "<RET>" #'evil-ex-nohighlight)
  (map! :map general-override-mode-map
        :n "C-j" #'evil-window-down
        :n "C-k" #'evil-window-up
        :n "C-h" #'evil-window-left
        :n "C-l" #'evil-window-right)
  (require 'evil-little-word))

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

(after! which-key
  (setq which-key-idle-delay 1))

(after! lsp-mode
  (require 'lsp-lua-sumneko)
  (setq lsp-auto-guess-root nil
        lsp-clients-emmy-lua-jar-path (expand-file-name (locate-user-emacs-file "EmmyLua-LS-all.jar"))
        lsp-ui-sideline-enable nil
        lsp-lua-sumneko-workspace-preload-file-size 100000
        lsp-lua-sumneko-workspace-max-preload 100000
        lsp-lua-sumneko-runtime-version "LuaJIT"))

(after! ivy
  (setq ivy-height 40))

(after! lua-mode
  (defun ruin/flycheck-locate-config-file-ancestor-directories (file _checker)
    (when-let ((path (flycheck-locate-config-file-ancestor-directories file _checker)))
      (subst-char-in-string ?/ ?\\ path)))
  (add-hook 'lua-mode-hook (lambda ()
                             (setq flycheck-locate-config-file-functions
                                   '(ruin/flycheck-locate-config-file-ancestor-directories))))


  (defun ruin/stylua-fmt-buffer ()
    (interactive)
    (shell-command (format "stylua --config-path %s %s"
                           (expand-file-name
                            "stylua.toml"
                            (projectile-locate-dominating-file (buffer-file-name) "stylua.toml"))
                           (buffer-file-name)))))

(defun ruin/set-major-mode-from-name (name)
  (let ((case-insensitive-p (file-name-case-insensitive-p name)))
    ;; Remove backup-suffixes from file name.
    (setq name (file-name-sans-versions name))
    ;; Remove remote file name identification.
    (while name
      ;; Find first matching alist entry.
      (setq mode
            (if case-insensitive-p
                ;; Filesystem is case-insensitive.
                (let ((case-fold-search t))
                  (assoc-default name auto-mode-alist
                                 'string-match))
              ;; Filesystem is case-sensitive.
              (or
               ;; First match case-sensitively.
               (let ((case-fold-search nil))
                 (assoc-default name auto-mode-alist
                                'string-match))
               ;; Fallback to case-insensitive match.
               (and auto-mode-case-fold
                    (let ((case-fold-search t))
                      (assoc-default name auto-mode-alist
                                     'string-match))))))
      (if (and mode
               (consp mode)
               (cadr mode))
          (setq mode (car mode)
                name (substring name 0 (match-beginning 0)))
        (setq name nil))
      (when mode
        (set-auto-mode-0 mode nil)))))

(defun ruin/view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point-min) (point))
    (delete-blank-lines)
    (ruin/set-major-mode-from-name url)))

(defun explorer ()
  "Open Finder or Windows Explorer in the current directory."
  (interactive)
  (cond
   ((string= system-type "darwin")
    (shell-command (format "open -b com.apple.finder %s"
                           (if (buffer-file-name)
                               (file-name-directory (buffer-file-name))
                             "~/"))))
   ((string= system-type "windows-nt")
    (start-process "explorer" nil "explorer"
                   (replace-regexp-in-string
                    "/" "\\\\"
                    (if (buffer-file-name)
                        (file-name-directory (buffer-file-name))
                      (expand-file-name  "~/")))))))

(define-key! help-map
  "h" #'helpful-at-point)

;; https://emacs.stackexchange.com/a/2838
(defun ruin/create-newline-and-allman-format (&rest _ignored)
  "Allman-style formatting for C."
  (interactive)
  (let ((line
         (save-excursion
           (previous-line 1)
           (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
    (newline-and-indent)
    (if (string-match-p "^[ \t]*{$" line)
        (progn
          (previous-line)
          (indent-according-to-mode))
      (progn
        (previous-line 2)
        (search-forward "{")
        (backward-char)
        (newline-and-indent)
        (next-line)
        (indent-according-to-mode)))))

(after! smartparens
  (sp-local-pair '(c-mode csharp-mode) "{" nil :post-handlers '((ruin/create-newline-and-allman-format "RET")))
  (map! :map lisp-mode-map
        :nv ">" #'sp-slurp-hybrid-sexp
        :nv "<" #'sp-forward-barf-sexp)
  (map! :map emacs-lisp-mode-map
        :nv ">" #'sp-slurp-hybrid-sexp
        :nv "<" #'sp-forward-barf-sexp)
  (map! :leader
        (:prefix-map ("l" . "lisp")
         ;; "c" #'sp-convolute-sexp
         ;; "C" #'lispy-convolute-left
         ;; "O" #'lispy-oneline
         ;; "M" #'lispy-alt-multiline
         ;; "S" #'lispy-stringify
         ;; "/" #'lispy-splice
         "r" #'sp-raise-sexp
         ;; "R" #'lispy-raise-some
         ;; "x" #'hydra-lispy-x/body
         ;; "p" #'lispy-clone
         )))

(use-package! clj-refactor
  :hook '(clojure-mode . clj-refactor-mode)
  :load-path "~/build/clj-refactor.el")

(after! magit
  (setq magit-clone-set-remote.pushDefault t
        magit-remote-add-set-remote.pushDefault t
        magit-commit-ask-to-stage nil
        magit-no-confirm '(stage-all-changes set-and-push)
        git-commit-summary-max-length 72)) ; GitHub max length

(define-key!
  "<f7>" #'previous-error-no-select
  "<f8>" #'next-error-no-select
  "M-p" #'flycheck-previous-error
  "M-n" #'flycheck-next-error)

(require 'hsp-mode)
(add-hook 'hsp-mode-hook (lambda ()
                           (add-to-list 'compilation-error-regexp-alist '("in line \\([0-9]+\\) \\[\\(.*?\\)\\]" 2 1))
                           (add-to-list 'compilation-error-regexp-alist '("^\\(.*?\\)(\\([0-9]+\\)) :" 1 2))
                           (define-key hsp-mode-map (kbd "C-j") nil)))

(after! rainbow-mode
  (add-to-list 'rainbow-html-rgb-colors-font-lock-keywords
               '("{\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*[0-9]*\.?[0-9]+\s*%?\s*}"
                 (0 (rainbow-colorize-rgb))))
  (add-to-list 'rainbow-html-rgb-colors-font-lock-keywords
               '("color(\s*\\([0-9]\\{1,3\\}\\(?:\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*)"
                 (0 (rainbow-colorize-rgb))))
  (add-to-list 'rainbow-html-rgb-colors-font-lock-keywords
               '("set_color(\s*\\([0-9]\\{1,3\\}\\(?:\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*\\(,\s*\\([0-9]\\{1,3\\}\\(?:\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*\\)?)"
                 (0 (rainbow-colorize-rgb))))
  (add-to-list 'rainbow-html-rgb-colors-font-lock-keywords
               '("{\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\)\s*}"
                 (0 (rainbow-colorize-rgb))))
  (add-to-list 'rainbow-html-rgb-colors-font-lock-keywords
               '("color \s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\)\s*"
                 (0 (rainbow-colorize-rgb))))
  (add-to-list 'rainbow-html-rgb-colors-font-lock-keywords
               '("SetColor(\s*\\([0-9]\\{1,3\\}\\(?:\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*\\(,\s*\\([0-9]\\{1,3\\}\\(?:\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*\\)?)"
                 (0 (rainbow-colorize-rgb))))
  (add-hook 'hsp-mode-hook 'rainbow-mode)
  (add-hook 'csharp-mode-hook 'rainbow-mode))


;;http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun ruin/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (when (get-buffer new-name)
      (kill-buffer new-name))
    (rename-buffer new-name)
    (when (file-exists-p filename)
      (rename-file filename new-name 1))
    (set-visited-file-name new-name)
    (set-buffer-modified-p nil)))

(defun ruin/move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil)
             t))))

(add-to-list 'auto-mode-alist '("\\.tpl?\\'" . mhtml-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.lua-format?\\'" . yaml-mode))

(setq js-indent-level 2)

(after! flycheck
  (add-to-list 'flycheck-checkers 'javascript-jshint))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/uim-el")
(when (locate-library "uim") (require 'uim)
      (global-set-key (kbd "C-\\") 'uim-mode)
      (setq uim-default-im-engine "anthy"))

(after! counsel
  (setq counsel-find-file-ignore-regexp "")
  (define-key!
    [remap +ivy/compile]         #'compile
    [remap +ivy/project-compile] #'projectile-compile-project))

(after! alchemist
  (map! :localleader
        :map elixir-mode-map
        :desc "Mix" "x" #'alchemist-mix
        :desc "Mix" "p" #'alchemist-iex-project-run
        :desc "Mix" "t" #'alchemist-mix-test
        (:prefix ("e" . "eval")
         "b" #'alchemist-iex-compile-this-buffer
         "l" #'alchemist-iex-send-current-line
         "r" #'alchemist-iex-send-region
         ))

  (defun save-buffer-trace (&rest args)
    (save-buffer))

  (advice-add 'alchemist-iex-compile-this-buffer :before #'save-buffer-trace))

(map! :leader
      (:prefix-map ("k" . "compile")
       :desc "Quickrun this file" "q" #'quickrun
       :desc "Recompile" "r" #'recompile
       :desc "Compile" "c" #'compile
       :desc "Kill compilation" "k" #'kill-compilation
       :desc "Compile project" "p" #'projectile-compile-project))


(if IS-WINDOWS
    (progn
      (add-to-list 'load-path (expand-file-name "C:/users/yuno/build/elona-next/editor/emacs"))
      (add-to-list 'load-path (expand-file-name "C:/users/yuno/build/OpenNefia.NET/Support")))
  (add-to-list 'load-path (expand-file-name "~/build/OpenNefia/editor/emacs")))
(when (locate-library "open-nefia")
  (require 'open-nefia)
  (after! open-nefia
    (setq lua-indent-level 4)
    ;(define-key lua-mode-map (kbd "M-:") #'open-nefia-eval-expression)
    (map! :localleader
          :map lua-mode-map
          "i" #'open-nefia-insert-require
          "I" #'open-nefia-insert-missing-requires
                                        ; "l" #'open-nefia-locale-search
                                        ; "L" #'open-nefia-locale-key-search
          "r" #'open-nefia-require-file
          "R" #'open-nefia-require-this-file
          "c" #'open-nefia-start-game
          "h" #'open-nefia-run-headlessly-repl
          (:prefix ("t" . "test")
           "a" #'open-nefia-run-tests
           "t" #'open-nefia-run-tests-this-file
           "r" #'open-nefia-run-previous-tests)
          (:prefix ("e" . "eval")
           "l" #'open-nefia-send-current-line
           "b" #'open-nefia-send-buffer
           "d" #'open-nefia-hotload-this-file
           "r" #'open-nefia-send-region
           "f" #'open-nefia-send-defun))
    )

  (require 'open-nefia-context)
  (after! open-nefia-context
    ;;(add-hook 'lua-mode-hook 'open-nefia-context-mode nil)
    (map! :localleader
          :map lua-mode-map
          (:prefix ("o" . "context")
           "g" #'open-nefia-context-goto
           "s" #'open-nefia-context-show))))

(when (locate-library "open-nefia-cs")
  (require 'open-nefia-cs)
  (after! open-nefia-cs
    (map! :localleader
          :map csharp-mode-map
          "h" #'open-nefia-cs-run-headlessly
          (:prefix ("e" . "eval")
           "l" #'open-nefia-cs-send-current-line
           "b" #'open-nefia-cs-send-buffer
           "r" #'open-nefia-cs-send-region
           "f" #'open-nefia-cs-send-defun))
    (map! :localleader
          :map lua-mode-map
          "l" #'open-nefia-cs-jump-to-other-locale-file)))

(after! elisp-mode
  (map! :localleader
        :map emacs-lisp-mode-map
        (:prefix ("e" . "eval")
         "s" #'eval-last-sexp
         "b" #'eval-buffer)))

(defun ruin/yank-path-of-buffer-file (&optional arg file)
  (interactive "P")
  (or file
      (setq file (buffer-file-name))
      (error "Current buffer has no file"))
  (let ((filename (if arg file (file-name-directory file))))
    (kill-new filename)
    (message filename)))

(defun ruin/yank-filename-of-buffer-file ()
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun ruin/regenerate-ltags ()
  (interactive)
  (when (projectile-project-p)
    (let ((default-directory (projectile-project-root)))
      (shell-command "ltags -nr -e **/*.lua")
      (when (projectile-project-p)
        (setq-local tags-file-name (string-join (list (projectile-project-root) "TAGS"))))
      (message "TAGS regenerated."))))

(after! lua-mode
  (setq compilation-error-regexp-alist (list (list lua-traceback-line-re 1 2)))
  (add-to-list 'compilation-error-regexp-alist '("^\\(.+\\):\\([1-9][0-9]*\\):\\([1-9][0-9]*\\): " 1 2 3))
  (add-to-list 'compilation-error-regexp-alist '(" in function <\\(.+\\):\\([1-9][0-9]+\\)>" 1 2))
  (evil-define-key 'normal compilation-mode-map (kbd "C-j") nil)
  (evil-define-key 'normal compilation-mode-map (kbd "C-k") nil)
  (add-hook 'lua-mode-hook (lambda ()
                             (set-company-backend! 'lua-mode '())
                             (company-mode 0)
                             (setq-local rainbow-html-colors t)
                             (lua-block-mode t)
                             (rainbow-mode 1))
            t)
  (require 'lua-block)
  (setq lua-block-highlight-toggle t)
  (map! :localleader
        :map lua-mode-map
        "A" #'ruin/regenerate-ltags)

  ;; doesn't work when required...
  (defun sp-lua-post-keyword-insert (id action _context)
    "ID, ACTION, CONTEXT."
    (cond
     ((eq action 'insert)
      (cond
       ((member id '("while" "for"))
        (insert " do")
        (save-excursion (newline-and-indent))
        (backward-char 3))
       ((equal id "if")
        (insert " then")
        (save-excursion (newline-and-indent))
        (backward-char 5))
       ((equal id "function")
        (save-excursion (newline-and-indent))
        (insert " "))))))

  ;; all the pairs are expanded only if followed by "SPC" event.  This
  ;; will reduce false positives like 'dIFficult' to trigger.
  (sp-with-modes '(lua-mode)
    (sp-local-pair "if" "end"
                   :when '(("SPC"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :post-handlers '(sp-lua-post-keyword-insert))
    (sp-local-pair "function" "end"
                   :when '(("SPC"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :post-handlers '(sp-lua-post-keyword-insert))
    (sp-local-pair "for" "end"
                   :when '(("SPC"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :post-handlers '(sp-lua-post-keyword-insert))
    (sp-local-pair "while" "end"
                   :when '(("SPC"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :post-handlers '(sp-lua-post-keyword-insert)))
  (set-company-backend! 'lua-mode '())
  (require 'smartparens-lua)
  (set-file-template! "^[A-Z].*\\.lua$" :trigger "__lua")
  (set-file-template! "^mod\\.lua$" :trigger "__mod")

  (add-to-list 'lua-font-lock-keywords
               '("\\_<\\([A-Z][A-Za-z0-9_]+\\)"
                 1 (unless (eq ?\( (char-after)) font-lock-type-face)) t)

  ;; (add-to-list 'lua-font-lock-keywords
  ;;              '("\\_<\\([A-Z][A-Z_]+\\)"
  ;;                1 (unless (eq ?\( (char-after)) font-lock-constant-face) t) t)
  )

(after! highlight-numbers
  (add-hook 'hsp-mode-hook #'highlight-numbers-mode))

(after! hl-line
  (if window-system
      (global-hl-line-mode 1)
    (progn
      (remove-hook 'text-mode-hook 'hl-line-mode)
      (remove-hook 'conf-mode-hook 'hl-line-mode)
      (remove-hook 'prog-mode-hook 'hl-line-mode)
      (global-hl-line-mode -1))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((projectile-project-run-cmd . "OpenNefia")
     (projectile-project-compilation-cmd . "OpenNefia")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key (kbd "C-x C-s") 'sort-lines)
(global-set-key (kbd "C-x |") 'align-regexp)

(defun ruin/refactor-name (&optional newsym)
  "Refactors the name at point in the current buffer unconditionally."
  (interactive)
  (let* ((sym (symbol-name (symbol-at-point)))
         (newsym (or newsym
                     (read-string (concat "Replace \"" sym "\" with: "))))
         (regexp (concat "\\_<\\(" (regexp-quote sym) "\\)\\_>")))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (while (re-search-forward regexp nil t)
          (replace-match newsym t nil))))))

(defun ruin/kill-term-buffer-on-exit ()
  "Hook for deleting the terminal window automatically."
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (lexical-let ((buff buff))
      (set-process-sentinel proc (lambda (process event)
                                   (if (string= event "finished\n")
                                       (kill-buffer buff)))))))

(add-hook 'term-exec-hook #'ruin/kill-term-buffer-on-exit)

(set-popup-rules!
  '(("^\\*ansi-term"
     :vslot -7 :side bottom :size 0.3 :select t :quit nil :ttl 0)))

(defun ruin/popup-term ()
  (interactive)
  (let ((+popup-default-display-buffer-actions
         '(+popup-display-buffer-stacked-side-window-fn))
        (display-buffer-alist +popup--display-buffer-alist)
        (buffer (save-window-excursion
                  (ansi-term shell-file-name)
                  (current-buffer))))
    (bury-buffer buffer)
    (pop-to-buffer buffer)))

(defun ruin/find-build ()
  (interactive)
  (counsel-find-file "~/build"))

(defun ruin/parent-dir (filename)
  "Return parent directory of absolute FILENAME."
  (file-name-directory (directory-file-name filename)))

(defun ruin/copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let* ((path (file-relative-name (buffer-file-name) (ruin/parent-dir (projectile-project-root))))
         (line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
         (path-with-line-number
          (format "-- >>>>>>>> %s:%d %s ..." path (line-number-at-pos)
                  (substring line 0 (min (length line) 50)))))
    (kill-new (propertize path-with-line-number 'yank-handler (list #'evil-yank-line-handler)))
    (message path-with-line-number)))

(defun ruin/copy-current-line-position-to-clipboard-2 ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let* ((path (file-relative-name (buffer-file-name) (ruin/parent-dir (projectile-project-root))))
         (line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
         (path-with-line-number
          (format "-- <<<<<<<< %s:%d %s ..." path (line-number-at-pos)
                  (substring line 0 (min (length line) 50)))))
    (kill-new (propertize path-with-line-number 'yank-handler (list #'evil-yank-line-handler)))
    (message path-with-line-number)))

(defun ruin/string-of-line-at-number (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

(defun ruin/copy-current-region-positions ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (when (not (region-active-p))
    (set-mark (point)))
  (let* ((path (file-relative-name (buffer-file-name) (ruin/parent-dir (projectile-project-root))))
         (bor-line (line-number-at-pos (region-beginning)))
         (eor-line (max bor-line (1- (line-number-at-pos (region-end)))))
         (bor-string (ruin/string-of-line-at-number bor-line))
         (eor-string (ruin/string-of-line-at-number eor-line))
         (path-with-line-number
          (format "-- >>>>>>>> %s:%d %s ...\n-- <<<<<<<< %s:%d %s ..."
                  path bor-line (substring bor-string 0 (min (length bor-string) 50))
                  path eor-line (substring eor-string 0 (min (length eor-string) 50)))))
    (kill-new (propertize path-with-line-number 'yank-handler (list #'evil-yank-line-handler)))
    (message path-with-line-number)
    (deactivate-mark)))

(require 'url)
(defun ruin/download-file (arg &optional url download-dir download-name)
  (interactive "P")
  (let ((url (or url
                 (read-string "Enter download URL: ")))
        (download-dir (or download-dir
                          (read-directory-name "Destination: " nil nil nil))))
    (let ((download-buffer (save-excursion (url-retrieve-synchronously url))))
      (unless (file-directory-p download-dir)
        (make-directory download-dir t))
      (with-current-buffer download-buffer
        ;; we may have to trim the http response
        (goto-char (point-min))
        (re-search-forward "^$" nil 'move)
        (forward-char)
        (delete-region (point-min) (point))
        (save-window-excursion
          (write-file (concat download-dir
                              (or download-name
                                  (car (last (split-string url "/" t))))))))
      (when arg (display-buffer download-buffer)))))

(require 'format-all)
(after! format-all
  (define-format-all-formatter stylua
    (:executable "stylua")
    (:install "cargo install stylua")
    (:languages "Lua")
    (:format
     (format-all--buffer-hard
      '(0 1) nil '("stylua.toml")
      executable "-"))))

(map! :leader
      :desc "Popup terminal" "\"" #'ruin/popup-term
      (:prefix-map ("p" . "project")
       :desc "Compile project" "c" #'projectile-compile-project)
      (:prefix-map ("y" . "yank")
       :desc "Yank kill ring" "y" #'counsel-yank-pop)
      (:prefix-map ("a" . "app")
       :desc "Calc" "c" #'calc
       :desc "Build regexp" "x" #'re-builder)
      (:prefix-map ("b" . "buffer")
       :desc "Format all" "f" #'format-all-buffer
       :desc "Yank buffer file name" "y" #'ruin/yank-filename-of-buffer-file
       :desc "Yank buffer path" "Y" #'ruin/yank-path-of-buffer-file)
      (:prefix-map ("y" . "yank")
       :desc "Yank line and file" "p" #'ruin/copy-current-line-position-to-clipboard
       :desc "Yank line and file end" "e" #'ruin/copy-current-line-position-to-clipboard-2
       :desc "Yank region lines" "y" #'ruin/copy-current-region-positions)
      (:prefix-map ("f" . "file")
       :desc "Find file from URL" "w" #'ruin/view-url
       :desc "Find build" "b" #'ruin/find-build
       :desc "Rename file and buffer" "R" #'ruin/rename-file-and-buffer
       :desc "Move buffer file" "M" #'ruin/move-buffer-file
       :desc "Download file" "o" #'ruin/download-file)
      (:prefix-map ("s" . "search")
       :desc "Search project (EX)" "P" #'+default/search-project-for-symbol-at-point))

(after! hl-todo
  (push '("BUG" error bold) hl-todo-keyword-faces))

(define-key global-map [remap compile] nil)
(define-key global-map [remap projectile-compile-project] nil)

(after! hi-lock
  (set-face-foreground 'hi-blue "#444")
  (set-face-foreground 'hi-yellow "#444")
  (set-face-foreground 'hi-pink "#444")
  (set-face-foreground 'hi-green "#444"))

(after! markdown-mode
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  (setq markdown-fontify-code-blocks-natively t))
(put 'narrow-to-region 'disabled nil)

(after! migemo
  (setq migemo-isearch-enable-p nil))

(when (eq window-system 'w32)
  (setq tramp-default-method "plink"))

(setq undo-fu-session-compression nil)

(setq plantuml-default-exec-mode 'jar
      plantuml-java-args (list "-D 'java.awt.headless=true'" "-jar"))

(after! company
  (setq evil-complete-next-func 'company-select-next)
  (setq evil-complete-previous-func 'company-select-previous)
  (setq company-backends '(company-files company-keywords company-capf)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)
    (merlin-eldoc-disable)
    (remove-hook 'merlin-mode-hook 'merlin-eldoc-setup)))

(setq js-indent-level 2)
(setq-default evil-shift-width 2)
                                        ; (advice-add 'eval-last-sexp :after #'save-buffer)
                                        ; (advice-add 'eval-defun :after #'save-buffer)
                                        ; (advice-add 'eval-buffer :after #'save-buffer)

(require 'teal-mode)
(after! teal-mode
  )

(require 'flycheck-tl)
(after! flycheck-tl
  (flycheck-tl-setup)
  (setq flycheck-lua-tl-include '("types/luafilesystem" "types" "types/luasocket")
        flycheck-lua-tl-load "main"))

(after! undo-tree
  (global-undo-tree-mode t)
  (add-hook 'prog-mode-hook #'turn-on-undo-tree-mode))

(map! :leader
      (:prefix ("r" . "replace")
       "r" #'projectile-replace
       "R" #'projectile-replace-regexp
       "s" #'ruin/refactor-name))

(defun ruin/pop-compilation-buffer ()
  (interactive)
  (let ((buf (save-window-excursion (compilation-goto-in-progress-buffer) (current-buffer))))
    (when (buffer-live-p buf)
      (+popup-buffer buf))))

(require 'lispy)
(defun ruin/lispy-read-expr-at-point ()
  (let* ((bnd (lispy--bounds-list))
         (str (lispy--string-dwim bnd)))
    (lispy--read str)))

(defun ruin/lispy-oneline-in-sexp ()
  (interactive)
  (save-excursion
    (let ((len (length (ruin/lispy-read-expr-at-point))))
      (down-list)
      (lispy-down 1)
      (dotimes (i len)
        (lispy-oneline)
        (lispy-down 1)))))

(defun ruin/format-adieu ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (lispy-forward 1)
    (lispy-backward 1)
    (indent-pp-sexp t)
    (down-list)
    (ruin/lispy-oneline-in-sexp)
    (lispy-down 1)
    (let ((len (length (ruin/lispy-read-expr-at-point))))
      (down-list)
      (lispy-down 1)
      (dotimes (i len)
        (down-list)
        (lispy-down 1)
        (ruin/lispy-oneline-in-sexp)
        (backward-up-list)
        (lispy-down 1)))))

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun ruin/translate-line ()
  (interactive)
  (beginning-of-line)
  (duplicate-line)
  (previous-line)
  (comment-line 1)
  (evil-find-char 1 (string-to-char "\"")))

(define-key evil-normal-state-map (kbd "C-t") 'ruin/translate-line)

(setq open-nefia-context-shade2-source-dir "~/build/poppy")

;; ispell
(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
  (setq ispell-program-name "aspell")
  (setq ispell-personal-dictionary (locate-user-emacs-file ".ispell"))
  (require 'ispell))

(after! monroe
  (setq monroe-default-host "localhost:3939"))

(after! ron-mode
  (setq ron-indent-offset 2))

(after! diff
  (setq diff-refine nil))

(cl-defun rmsbolt--rustic-compile-cmd (&key src-buffer)
  "Process a compile command for rustic."
  (rmsbolt--with-files
   src-buffer
   (let* ((src-filename (string-replace "\\:/" ":/" src-filename))
          (output-filename (string-replace "\\:/" ":/" output-filename))
          (asm-format (buffer-local-value 'rmsbolt-asm-format src-buffer))
          (disass (buffer-local-value 'rmsbolt-disassemble src-buffer))
          (cmd (buffer-local-value 'rmsbolt-command src-buffer))
          (cargo-toml (locate-dominating-file src-filename "Cargo.toml"))
          (cmd (mapconcat #'identity
                          (list
                           cmd
                           "rustc"
                           (when cargo-toml "--manifest-path")
                           (when cargo-toml (expand-file-name "Cargo.toml" cargo-toml))
                           "--release"
                           "--"
                           "-g"
                           "--emit"
                           (if disass
                               "link"
                             "asm")
                           ;; src-filename
                           "-o" output-filename
                           (when (and (not (booleanp asm-format))
                                      (not disass))
                             (concat "-Cllvm-args=--x86-asm-syntax=" asm-format)))
                          " ")))
     (when cargo-toml
       (setq-local rmsbolt-default-directory cargo-toml))
     cmd)))


(after! rmsbolt
  (require 'rmsbolt)
  (let ((lang (make-rmsbolt-lang :compile-cmd "cargo"
                                 :supports-asm t
                                 :supports-disass nil
                                 :objdumper 'objdump
                                 :demangler "rustfilt"
                                 :compile-cmd-function #'rmsbolt--rustic-compile-cmd)))
    (add-to-list 'rmsbolt-languages `(rustic-mode . ,lang))))

(defun xah-open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app.

The app is chosen from your OS's preference."
  (interactive)
  (let (doIt
        (myFileList
         (cond
          ((string-equal major-mode "dired-mode") (dired-get-marked-files))
          ((not file) (list (buffer-file-name)))
          (file (list file)))))

    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ") ) )

    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )

(after! json
  (setq-default flycheck-disabled-checkers '(json-python-json)))

(when (featurep! :tools lookup)
  (map! :leader
        (:prefix ("c" . "code")
         "r" #'+lookup/references))

  (map! :nv "gr" #'+lookup/references
        :nv "gi" #'+lookup/impementations
        :nv "ga" #'+lookup/assignments))

(map! :leader
      (:prefix ("r" . "replace")
       "r" #'lsp-rename)
      (:prefix ("f" . "file")
       "B" #'explorer))

(after! counsel
  (advice-add 'counsel-rg
              :around
              (lambda (func &rest args)
                (cl-letf (((symbol-function #'process-exit-status)
                           (lambda (_proc) 0)))
                  (apply func args)))))

(after! indent-tools
  (require 'indent-tools)
  (add-hook 'yaml-mode-hook #'indent-tools-minor-mode))

(after! ron-mode
  (require 'ron-mode))

(after! csharp-mode
  (c-add-style
   "ruin" '((c-comment-only-line-offset . 0)
            (c-hanging-braces-alist (brace-list-open)
                                    (brace-entry-open)
                                    (substatement-open before after)
                                    (block-close . c-snug-do-while)
                                    (arglist-cont-nonempty))
            (c-cleanup-list brace-else-brace)
            (c-offsets-alist
             (knr-argdecl-intro . 0)
             (substatement-open . 0)
             (substatement-label . 0)
             (statement-cont . +)
             (case-label . +)
             ;; align args with open brace OR don't indent at all (if open
             ;; brace is at eolp and close brace is after arg with no trailing
             ;; comma)
             (brace-list-intro . 0)
             (brace-list-close . -)
             (arglist-intro . +)
             (arglist-close +cc-lineup-arglist-close 0)
             ;; don't over-indent lambda blocks
             (inline-open . 0)
             (inlambda . 0)
             ;; indent access keywords +1 level, and properties beneath them
             ;; another level
             (access-label . -)
             (inclass +cc-c++-lineup-inclass +)
             (label . 0))))

  (when (listp c-default-style)
    (setf (alist-get 'csharp-mode c-default-style) "ruin")))
