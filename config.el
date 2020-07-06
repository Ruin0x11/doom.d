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
(setq doom-font (font-spec :family "Kochi Gothic" :size 12))

(defun ruin/init-cjk-font ()
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset doom-font)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (ruin/init-cjk-font)))
  (ruin/init-cjk-font))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

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
  (setq evil-ex-substitute-global nil)
  (map! :leader
        "<RET>" #'evil-ex-nohighlight)
  (map!
   :g "C-h" #'evil-window-left
   :g "C-l" #'evil-window-right
   :g "C-k" #'evil-window-up
   :g "C-j" #'evil-window-down))

(after! evil-snipe
  (evil-snipe-mode -1))

(after! which-key
  (setq which-key-idle-delay 0.2))

(after! lsp-mode
  (require 'lsp-lua-sumneko)
  (setq lsp-auto-guess-root nil
        lsp-clients-emmy-lua-jar-path (expand-file-name (locate-user-emacs-file "EmmyLua-LS-all.jar"))
        lsp-lua-sumneko-workspace-preload-file-size 100000
        lsp-lua-sumneko-workspace-max-preload 100000
        lsp-lua-sumneko-runtime-version "LuaJIT"))

(after! ivy
  (when IS-WINDOWS
    (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s --path-separator // .")))

(after! projectile
  (when (executable-find doom-projectile-fd-binary)
    (setq projectile-generic-command
          (concat (format "%s . -0 -H -E .git --color=never --type file --type symlink --follow"
                          doom-projectile-fd-binary)
                  (if IS-WINDOWS " --path-separator=//")))))


(after! lua-mode
  (defun ruin/flycheck-locate-config-file-ancestor-directories (file _checker)
    (when-let ((path (flycheck-locate-config-file-ancestor-directories file _checker)))
      (subst-char-in-string ?/ ?\\ path)))
  (add-hook 'lua-mode-hook (lambda ()
                             (setq flycheck-locate-config-file-functions
                                   '(ruin/flycheck-locate-config-file-ancestor-directories))))
  (add-hook 'lua-mode-hook #'lsp!))

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
    (set-auto-mode)))

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
    (shell-command (format "explorer %s"
			   (replace-regexp-in-string
			    "/" "\\\\"
			    (if (buffer-file-name)
				(file-name-directory (buffer-file-name))
			      (expand-file-name  "~/"))))))))

(define-key! help-map
  "h" #'helpful-at-point)

(map! :leader
      (:prefix-map ("f" . "file")
     :desc "Find file from URL" "w" #'ruin/view-url))

(after! smartparens
  (map! :map lisp-mode-map
        :ni ">" #'sp-slurp-hybrid-sexp
        :ni "<" #'sp-forward-barf-sexp)
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

(use-package! cider
  :hook '((clojure-mode lua-mode) . cider-mode)
  :load-path "~/build/cider")

(after! cider
  (map! :leader
        (:prefix-map ("p" . "project")
        :desc "sesman" "S" sesman-map)))

(use-package! clj-refactor
  :hook '(clojure-mode . clj-refactor-mode)
  :load-path "~/build/clj-refactor.el")

(after! magit
  (setq magit-clone-set-remote.pushDefault t
        magit-remote-add-set-remote.pushDefault t
        magit-commit-ask-to-stage nil))

(map! :leader "co" #'recompile)
(define-key!
  "<f7>" #'previous-error
  "<f8>" #'next-error)

(require 'hsp-mode)
(add-hook 'hsp-mode-hook (lambda ()
                           (add-to-list 'compilation-error-regexp-alist '("in line \\([0-9]+\\) \\[\\(.*?\\)\\]" 2 1))
                           (add-to-list 'compilation-error-regexp-alist '("^\\(.*?\\)(\\([0-9]+\\)) :" 1 2))
                           (define-key hsp-mode-map (kbd "C-j") nil)))

(use-package! rainbow-mode)
(after! rainbow-mode
  (add-to-list 'rainbow-html-rgb-colors-font-lock-keywords
               '("{\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*[0-9]*\.?[0-9]+\s*%?\s*}"
                 (0 (rainbow-colorize-rgb))))
  (add-to-list 'rainbow-html-rgb-colors-font-lock-keywords
               '("color(\s*\\([0-9]\\{1,3\\}\\(?:\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*)"
                 (0 (rainbow-colorize-rgb))))
  (add-to-list 'rainbow-html-rgb-colors-font-lock-keywords
               '("set_color(\s*\\([0-9]\\{1,3\\}\\(?:\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*)"
                 (0 (rainbow-colorize-rgb))))
  (add-to-list 'rainbow-html-rgb-colors-font-lock-keywords
               '("{\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\)\s*}"
                 (0 (rainbow-colorize-rgb))))
  (add-to-list 'rainbow-html-rgb-colors-font-lock-keywords
               '("color \s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\)\s*"
                 (0 (rainbow-colorize-rgb))))
  (add-hook 'lua-mode-hook 'rainbow-mode)
  (add-hook 'hsp-mode-hook 'rainbow-mode))


;;http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
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

(defun move-buffer-file (dir)
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

(use-package! forge
  :after magit)

(add-to-list 'auto-mode-alist '("\\.tpl?\\'" . mhtml-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . js-mode))

(after! flycheck
  (add-to-list 'flycheck-checkers 'javascript-jshint))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/uim-el")
(when (locate-library "uim") (require 'uim)
      (global-set-key (kbd "C-\\") 'uim-mode)
      (setq uim-default-im-engine "anthy"))

(after! counsel
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

(after! quickrun
  (map! :leader
        (:prefix-map ("c" . "compile")
         :desc "Quickrun this file" "q" #'quickrun)))

(add-to-list 'load-path (expand-file-name "~/build/elona-next/editor/emacs"))
(when (locate-library "open-nefia")
  (require 'open-nefia)
  (after! open-nefia
    (setq lua-indent-level 3)
    (map! :localleader
          :map lua-mode-map
          "i" #'open-nefia-insert-require
          "r" #'open-nefia-require-this-file
          "p" #'open-nefia-start-repl
          "c" #'open-nefia-start-game
          (:prefix ("e" . "eval")
           "l" #'open-nefia-send-current-line
           "b" #'open-nefia-send-buffer
           "d" #'open-nefia-hotload-this-file
           "r" #'open-nefia-send-region
           "f" #'open-nefia-send-defun))
    ))

(after! elisp-mode
  (map! :localleader
        :map emacs-lisp-mode-map
        (:prefix ("e" . "eval")
         "s" #'eval-last-sexp)))

(defun ruin/yank-path-of-buffer-file (&optional arg file)
  (interactive "P")
  (or file
      (setq file (buffer-file-name))
      (error "Current buffer has no file"))
  (let ((filename (if arg file (file-name-directory file))))
    (kill-new filename)
    (message filename)))

(after! lua-mode
  (setq compilation-error-regexp-alist (list (list lua-traceback-line-re 1 2)))
  (add-to-list 'compilation-error-regexp-alist
               '(" in function <\\(.+\\):\\([1-9][0-9]+\\)>" 1 2)))

(after! highlight-numbers
  (add-hook 'hsp-mode-hook #'highlight-numbers-mode))
