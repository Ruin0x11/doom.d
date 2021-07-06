;;; hsp-mode.el --- mode for editing Hot Soup Processor code

;; Copyright (C) 1991 Free Software Foundation, Inc.

;; Author: Iwata <iwata@quasiquote.org>
;; Keywords: tools, languages

;; 	@(#)hsp-mode.el 0.1

;; Adapted from assembler code editing commands 'asm-mode.el',
;; Copyright 1991 by the Free Software Foundation, under terms of its
;; General Public License.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; Add your ~/.emacs

;; (setq auto-mode-alist
;;      (cons (cons "\\.as$" 'hsp-mode) auto-mode-alist))

;; And you have to set path of help(*.html) files
;; (setq hsp-help-location "/usr/local/lib/hsp/hsphelp")
;; For WIN32 netscape, you have to put "//" before pathname.
;; (setq hsp-help-location "//c:/hsp/hsphelp")

;; Set autoload
;; (autoload 'hsp-mode "hsp-mode" "Hot Soup Processor mode" t)

;; Keymap

;;  * C-c ?		View keyword from help file with WWW browser

;;; Code:

(provide 'hsp-mode)

(require 'browse-url)

(defconst hsp-mode-version "0.1"
  "Version of `hsp-mode.el'.")

(defvar hsp-shell-command-option
  (or (and (boundp 'shell-command-option) shell-command-option)
      (if (eq system-type 'ms-dos) "/c" "-c")))

(defvar hsp-help-location "/usr/local/lib/hsp"
  "*Path name of HSP help sets")

(defvar hsp-comment-char ?;
  "*The comment-start character assumed by HSP mode.")

(defconst hsp-multi-statement-always-indent t
  "*Non-nil means TAB in HSP mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")

(defcustom hsp-indent-level 4
  "The tab width to use when indenting."
  :type 'integer)

(defvar hsp-mode-abbrev-table nil
  "Abbrev table used while in HSP mode.")
(define-abbrev-table 'hsp-mode-abbrev-table ())

(defvar hsp-mode-map nil
  "Keymap for HSP mode.")

(if hsp-mode-map
    nil
  (setq hsp-mode-map (make-sparse-keymap))
  (define-key hsp-mode-map ";"		'hsp-comment)
  (define-key hsp-mode-map "\t"		'hsp-indent-command)
  (define-key hsp-mode-map "\C-j"	'hsp-newline)
  (define-key hsp-mode-map "\C-m"	'hsp-newline)
  (define-key hsp-mode-map "\C-c?"	'hsp-help))

(defvar hsp--face nil)
(defvar-local hsp--tags-table nil)

(defvar hsp-tags-completion-table-function #'hsp-elisp-tags-completion-table-function)

(defun hsp-elisp-tags-completion-table-function ()
  (let (table
        (progress-reporter
         (make-progress-reporter
          (format "Making tags completion table for %s..." buffer-file-name)
          (point-min) (point-max))))
    (save-excursion
      (goto-char (point-min))
      ;; This regexp matches an explicit tag name or the place where
      ;; it would start.
      (while (re-search-forward
              "[\f\t\n\r()=,; ]?\177\\\(?:\\([^\n\001]+\\)\001\\)?\\([^\n]+\\)?,\\([0-9]+\\)"
              nil t)
        (push (prog1
                  ;; There is an explicit tag name.
                  (list :explicit (when (match-beginning 1) (match-string-no-properties 1))
                        :text (match-string-no-properties 2)
                        :implicit (progn
                                    ;; No explicit tag name.  Backtrack a little,
                                    ;; and look for the implicit one.
                                    (goto-char (match-beginning 0))
                                    (skip-chars-backward "^\f\t\n\r()=,; ")
                                    (prog1
                                        (buffer-substring (point) (match-beginning 0))
                                      (goto-char (match-end 0)))))
                  (progress-reporter-update progress-reporter (point)))
                table)))
    table) )

(defvar-local hsp-tags-completion-table nil)

(defun hsp-tags-completion-table (&optional buf)
  "Build `tags-completion-table' on demand for a buffer's tags tables.
Optional argument BUF specifies the buffer for which to build
\`tags-completion-table', and defaults to the current buffer.
The tags included in the completion table are those in the current
tags table for BUF and its (recursively) included tags tables."
  (if (not buf) (setq buf (current-buffer)))
  (with-current-buffer buf
    (or hsp-tags-completion-table
        ;; No cached value for this buffer.
        (condition-case ()
            (let (tables cont)
              (message "Making tags completion table for %s..."
                       buffer-file-name)
              (save-excursion
                ;; Iterate over the current list of tags tables.
                (while (visit-tags-table-buffer cont buf)
                  ;; Find possible completions in this table.
                  (push (funcall hsp-tags-completion-table-function) tables)
                  (setq cont t)))
              (message "Making tags completion table for %s...done"
                       buffer-file-name)
              ;; Cache the result in a variable.
              (setq hsp-tags-completion-table
                    (nreverse (delete-dups (apply #'nconc tables)))))
          (quit (message "Tags completion table construction aborted.")
                (setq hsp-tags-completion-table nil))))))

(defconst hsp--tag-type-regexps
  '(("#enum global" . font-lock-type-face)
    ("#define global ctype" . font-lock-variable-name-face)
    ("#define global [a-zA-Z_]+(" . font-lock-function-name-face)
    ("#define global" . font-lock-constant-face)
    ("#define [a-zA-Z_]+(" . font-lock-function-name-face)
    ("#def[c]?func" . font-lock-function-name-face)
    ("^\\*[a-zA-Z_]+$" . font-lock-function-name-face)
    ))

(defun hsp--tag-type (tag)
  (if-let ((ty (seq-find (lambda (pair)
                           (string-match-p (car pair) (plist-get tag :text)))
                         hsp--tag-type-regexps)))
      (cdr ty)
    font-lock-variable-name-face))

(defun hsp--make-tags-table ()
  (if hsp--tags-table hsp--tags-table
    (let ((hash (make-hash-table))
          (tags (hsp-tags-completion-table (current-buffer))))
      (mapc (lambda (tag) (puthash (intern (downcase (plist-get tag :explicit))) (cons tag (hsp--tag-type tag)) hash)) tags)
      (setq-local hsp--tags-table hash)
      hash)))

(defun hsp-highlight-vars (end)
  (let ((tags (hsp--make-tags-table)))
    (catch 'matcher
      (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" end t)
        (let ((ppss (save-excursion (syntax-ppss))))
          (cond ((nth 3 ppss)  ; strings
                 (search-forward "\"" end t))
                ((nth 4 ppss)  ; comments
                 (forward-line +1))
                ;; hsp has case-insensitive identifiers...
                ((let ((symbol (intern-soft (downcase (match-string-no-properties 0)))))
                   (when-let ((ty (gethash symbol tags)))
                     (setq hsp--face (cdr ty))
                     (throw 'matcher t)))))))
      nil)))

(dolist (fn '(hsp--make-tags-table hsp-highlight-vars hsp--tag-type))
  (unless (byte-code-function-p (symbol-function fn))
    (with-no-warnings (byte-compile fn))))

(defconst hsp-font-lock-keyword-beg-re "\\(?:^\\|[^.@$:]\\|\\.\\.\\)")

(defconst hsp-font-lock-keywords
  `((";.*$"
     0 font-lock-comment-face)
    ("//.*$"
     0 font-lock-comment-face)
    (,(concat
       (regexp-opt
        '("if"
          "or"
          "then"
          "else"
          "goto"
          "gosub"
          "return"
          "repeat"
          "loop"
          "stop"
          "end"
          "break"
          "continue"
          "_continue"
          "switch"
          "case"
          "swbreak"
          "swend"
          "call"
          "default"
          "while"
          "wend"
          )
        'symbols))
     (0 font-lock-keyword-face))
    ;; label started from "*"
    ("\\*[a-zA-Z][_a-zA-Z0-9]+"
     0 font-lock-constant-face)
    (":"
     0 font-lock-keyword-face)
    (,(concat
       hsp-font-lock-keyword-beg-re
       (regexp-opt
        '("int"
          "str"
          "double")
        'symbols))
     (1 font-lock-type-face))
    ("^[ \t]*\\(#.*\n\\)"
     0 font-lock-preprocessor-face)
    ;; obsolate
    (,(concat
       hsp-font-lock-keyword-beg-re
       (regexp-opt
        '("refstr"
          "curdir"
          "windir"
          "csry"
          "csrx"
          "strsize"
          "winy"
          "winx"
          "cmdline"
          "bval"
          "gval"
          "rval"
          "mousey"
          "mousex"
          "gmode"
          "dispy"
          "dispx"
          "stat"
          "err"
          "cnt"
          "hspver"
          "system")
        'symbols))
     (1 font-lock-variable-name-face))
    (,(concat
       hsp-font-lock-keyword-beg-re
       (regexp-opt
        '("#func"
          "#defcfunc"
          "#deffunc")
        'symbols))
     (1 font-lock-keyword-face))
    ("^\\s *#def[c]?func\\s +\\(?:[^( \t\n.]*\\.\\)?\\([^( \t\n]+\\)"
     1 font-lock-function-name-face)
    (,(concat hsp-font-lock-keyword-beg-re
              "\\_<\\(true\\|false\\|falseM\\)\\_>")
     1 font-lock-constant-face)
    (hsp-highlight-vars 0 hsp--face)
    )
  "Additional expressions to highlight in HSP mode.")

(put 'hsp-mode 'font-lock-defaults '(hsp-font-lock-keywords nil t))

(defvar hsp-imenu-generic-expression
  '(("Function"  "^\\s *#def[c]?func\\s +\\(?:[^( \t\n.]*\\.\\)?\\([^( \t\n]+\\)" 1)
    ("Label"  "^\\*\\([a-zA-Z][_a-zA-Z0-9]+\\)" 1)))

(defvar hsp-mode-syntax-table nil
  "Syntax table used while in HSP mode.")

(unless hsp-mode-syntax-table
  (setq hsp-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\"  hsp-mode-syntax-table)
  (modify-syntax-entry ?¥ "\\"  hsp-mode-syntax-table)
  (modify-syntax-entry ?( "()1"  hsp-mode-syntax-table)
		       (modify-syntax-entry ?) ")(4"  hsp-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" hsp-mode-syntax-table)
  (modify-syntax-entry ?+ "."    hsp-mode-syntax-table)
  (modify-syntax-entry ?- "."    hsp-mode-syntax-table)
  (modify-syntax-entry ?= "."    hsp-mode-syntax-table)
  (modify-syntax-entry ?% "."    hsp-mode-syntax-table)
  (modify-syntax-entry ?< "."    hsp-mode-syntax-table)
  (modify-syntax-entry ?> "."    hsp-mode-syntax-table)
  (modify-syntax-entry ?& "."    hsp-mode-syntax-table)
  (modify-syntax-entry ?| "."    hsp-mode-syntax-table))

(defvar hsp-code-level-empty-comment-pattern nil)
(defvar hsp-flush-left-empty-comment-pattern nil)
(defvar hsp-inline-empty-comment-pattern nil)


(defun hsp-set-imenu-generic-expression ()
  (make-local-variable 'imenu-generic-expression)
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  (setq imenu-generic-expression hsp-imenu-generic-expression))

(add-hook 'hsp-mode-hook 'hsp-set-imenu-generic-expression)

(defun hsp-line-matches (pattern &optional withcomment)
  (save-excursion
    (beginning-of-line)
    (looking-at pattern)))

(defun hsp-pop-comment-level ()
  ;; Delete an empty comment ending current line.  Then set up for a new one,
  ;; on the current line if it was all comment, otherwise above it
  (end-of-line)
  (delete-horizontal-space)
  (while (= (preceding-char) hsp-comment-char)
    (delete-backward-char 1))
  (delete-horizontal-space)
  (if (bolp)
      nil
    (beginning-of-line)
    (open-line 1)))


(defun hsp-comment ()
  "Convert an empty comment to a `larger' kind, or start a new one.
These are the known comment classes:

   1 -- comment to the right of the code (at the comment-column)
   2 -- comment on its own line, indented like code
   3 -- comment on its own line, beginning at the left-most column.

Suggested usage:  while writing your code, trigger hsp-comment
repeatedly until you are satisfied with the kind of comment."
  (interactive)
  (cond

   ;; Blank line?  Then start comment at code indent level.
   ((hsp-line-matches "^[ \t]*$")
    (delete-horizontal-space)
    (tab-to-tab-stop)
    (insert hsp-comment-char comment-start))

   ;; Nonblank line with no comment chars in it?
   ;; Then start a comment at the current comment column
   ((hsp-line-matches (format "^[^%c\n]+$" hsp-comment-char))
    (indent-for-comment))

   ;; Flush-left comment present?  Just insert character.
   ((hsp-line-matches hsp-flush-left-empty-comment-pattern)
    (insert hsp-comment-char))

   ;; Empty code-level comment already present?
   ;; Then start flush-left comment, on line above if this one is nonempty.
   ((hsp-line-matches hsp-code-level-empty-comment-pattern)
    (hsp-pop-comment-level)
    (insert hsp-comment-char hsp-comment-char comment-start))

   ;; Empty comment ends line?
   ;; Then make code-level comment, on line above if this one is nonempty.
   ((hsp-line-matches hsp-inline-empty-comment-pattern)
    (hsp-pop-comment-level)
    (tab-to-tab-stop)
    (insert hsp-comment-char comment-start))

   ;; If all else fails, insert character
   (t
    (insert hsp-comment-char))
   )
  (end-of-line))

(defun hsp-indent-command (&optional whole-exp)
  "HSP-format indent."
  (interactive "P")
  (if whole-exp
      ((tab-to-tab-stop)
       (hsp-indent-line))
    (if
	;; Check non-nil line
	(and (bolp) (eolp))
	(tab-to-tab-stop)
      (skip-chars-forward "\t"))
    (hsp-indent-line)))

(defsubst hsp--paren-level ()
  (car (syntax-ppss)))

(defsubst hsp--in-string-or-comment-p ()
  (nth 8 (syntax-ppss)))

(defun hsp--indent-one-line ()
  "Indent current line as HSP code.
Return the amount the indentation changed by."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Indent preprocess or label
     ((save-excursion (looking-at "\t*[\\\*#]"))
      (delete-horizontal-space))
     ((save-excursion (looking-at "\t\t+"))
      (delete-horizontal-space)
      (tab-to-tab-stop))
     ;; Skip comment ";;;"
     ((save-excursion (looking-at "^;;;"))
      ())
     ((save-excursion (looking-at "[^\t]"))
      (tab-to-tab-stop)))))

(defun hsp--block-indentation ()
  (let ((curline (line-number-at-pos)))
    (save-excursion
      (condition-case nil
          (progn
            (backward-up-list)
            (unless (= curline (line-number-at-pos))
              (current-indentation)))
        (scan-error
         (save-excursion
           (beginning-of-line)
           (if (looking-at "^\\*") (- hsp-indent-level) (- hsp-indent-level hsp-indent-level)))
         )))))

(defun hsp--previous-indentation ()
  (save-excursion
    (forward-line -1)
    (let (finish)
      (while (not finish)
        (cond ((bobp) (setq finish t))
              ((hsp--in-string-or-comment-p) (forward-line -1))
              (t
               (let ((line (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))))
                 (if (not (string-match-p "\\`\\s-*\\'" line))
                     (setq finish t)
                   (forward-line -1))))))
      (current-indentation))))

(defun hsp-indent-line ()
  "Indent current line as HSP."
  (interactive)
  (let* ((curpoint (point))
         (pos (- (point-max) curpoint)))
    (back-to-indentation)
    (if (hsp--in-string-or-comment-p)
        (goto-char curpoint)
      (let ((block-indentation (hsp--block-indentation)))
        (delete-region (line-beginning-position) (point))
        (if block-indentation
            (if (looking-at "[]}]")
                (indent-to block-indentation)
              (indent-to (+ block-indentation hsp-indent-level)))
          (indent-to (hsp--previous-indentation)))
        (when (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos)))))))

(defun hsp-newline ()
  "Insert LFD + fill-prefix, to bring us back to code-indent level."
  (interactive)
  (if (eolp) (delete-horizontal-space))
  (hsp-indent-line)
  (newline)
  (hsp-indent-line)
  ;; Empty line
  (if (eolp)
      (tab-to-tab-stop)
    (skip-chars-forward "\t")))

(defun hsp-help ()
  "Show help buffer of HSP commands or macros."
  (interactive)
  (let (p beg end command command-prefix help-file)
    (save-excursion
      ;; Get command name
      (setq p (point))			; Remember current position.
      ;; !!!
      (if (forward-word -1)
	  (setq beg (point)))
      (if (forward-word 1)
	  (setq end (point)))
      (if (and (<= beg p) (<= p end))
	  (setq command (buffer-substring beg end)))
      (setq command
	    (read-from-minibuffer "Manual entry: " command))
      (setq command-prefix (substring command 0 1))
      ;; Do you want to access help documents via http?
      ;; Then change this line, "file:" to "http:"
      (setq help-file (concat "file:" hsp-help-location "/help_" command-prefix ".htm#s_" command))
      ;; Starting process of HTML browser
      (if (or (eq browse-url-browser-function 'browse-url-w3)
	      (eq browse-url-browser-function 'w3m-browse-url))
	  (split-window))
	(browse-url help-file))))

(defun hsp-mode ()
  "A major editing mode for the language Hot Soup Processor(HSP)."
  (interactive)
  (kill-all-local-variables)
  (use-local-map hsp-mode-map)
  (setq major-mode 'hsp-mode)
  (setq mode-name "HSP")
  (setq local-abbrev-table hsp-mode-abbrev-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(hsp-font-lock-keywords))
  (set-syntax-table hsp-mode-syntax-table)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)
  (setq indent-line-function 'hsp-indent-line)
  (run-hooks 'hsp-mode-set-comment-hook)
  (modify-syntax-entry	hsp-comment-char
			"<" hsp-mode-syntax-table)
  (modify-syntax-entry	?\n
			">" hsp-mode-syntax-table)
  (let ((cs (regexp-quote (char-to-string hsp-comment-char))))
    (make-local-variable 'comment-start)
    (setq comment-start (concat cs " "))
    (make-local-variable 'comment-start-skip)
    (setq comment-start-skip (concat cs "+[ \t]*"))
    (setq hsp-inline-empty-comment-pattern (concat "^.+" cs "+ *$"))
    (setq hsp-code-level-empty-comment-pattern (concat "^[\t ]+" cs cs " *$"))
    (setq hsp-flush-left-empty-comment-pattern (concat "^" cs cs cs " *$"))
    )
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (setq abbrev-all-caps t)
  (run-hooks 'hsp-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hsp\\'" . hsp-mode))

;;; hsp-mode.el ends here
