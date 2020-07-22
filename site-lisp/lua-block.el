;;; lua-block.el --- highlight matching block

;; Copyright (C) 2007-2013 khiker, shishi, juszczakn

;; Author: khiker <khiker.mail+elisp@gmail.com>
;; Keywords: languages, faces, lua

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.         See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.        If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Usage:

;; Add this line to your .emacs
;;
;; (require 'lua-block)
;; (lua-block-mode t)
;;
;; In addition, you can also add this line too.
;;
;; ;; do overlay
;; (setq lua-block-highlight-toggle 'overlay)
;; ;; display to minibuffer
;; (setq lua-block-highlight-toggle 'minibuffer)
;; ;; display to minibuffer and do overlay
;; (setq lua-block-highlight-toggle t)
;;
;; Default is minibuffer.
;;
;; Tested on Emacs 24.3

;;; Note:

;;; Code:

(require 'lua-mode)

;; Variables:

(defconst lua-block-version "0.0.11"
  "Lua block package version.")

(defconst lua-block-keyword-list
  (list "end" "while" "repeat" "if" "function" "do")
  "Keywords for highlighting.")

(defconst lua-block-keyword-regex
  (regexp-opt lua-block-keyword-list 'symbols)
  "Regular expression to look for correspondence.")

(defgroup lua-block nil
  "Lua block"
  :tag "Lua block"
  :group 'lua-block)

(defcustom lua-block-delay 0.50
  "*Time in seconds to delay before showing a matching paren."
  :type         'number
  :group 'lua-block)

(defcustom lua-block-highlight-face 'highlight
  "*Face for block highlighting."
  :type         'face
  :group 'lua-block)

(defcustom lua-block-highlight-toggle 'minibuffer
  "*How to display corresponding line.
Default is minibuffer. display to minibuffer.

The choices are as follows.

nil         => nothing
minibuffer => minibuffer
overlay         => overlay
t         => minibuffer and overlay"
  :type '(choice (const :tag "nothing" nil)
				 (const :tag "minibuffer" minibuffer)
				 (const :tag "overlay" overlay)
				 (const :tag "minibuffer and overlay" t))
  :group 'lua-block)

(defvar lua-block-timer nil)

(defvar lua-block-highlight-overlay nil)


;; Functions:

(define-minor-mode lua-block-mode
  "In lua-mode, Displays the line where there is a keyword corresponding
to END keyword.
This is a minor-mode for lua-mode and enh-lua-mode only."
  :init-value t
  :global nil
  :keymap nil
  :lighter " LBlock"
  (if lua-block-mode
      (lua-block-start-timer)
    (lua-block-stop-timer)))

(defun lua-block-start-timer ()
  "start timer."
  (when lua-block-timer
    (cancel-timer lua-block-timer))
  (setq lua-block-timer
		(run-with-idle-timer lua-block-delay t 'lua-block-hook)))

(defun lua-block-stop-timer ()
  "Stop timer."
  (when lua-block-timer
    (cancel-timer lua-block-timer)
    (setq lua-block-timer nil)))

(defun lua-block-hook ()
  "When Major-mode is lua-mode or enh-lua-mode, this package is running."
  (if (eq major-mode 'lua-mode)
      (condition-case err
		  (lua-block-function)
		(error
		 (setq lua-block-mode nil)
		 (message "Error: %S; lua-block-mode now disabled." err)))
    (setq lua-block-mode nil)))

(defun lua-block-line-beginning-position (pos)
  (when pos
    (save-excursion
      (goto-char pos)
      (let ((xor '(lambda (a b) (and (or a b) (not (and a b)))))
			(pos (point))
			(count 0))
		(while (and (not (funcall xor (bobp) (eolp)))
					(> pos (point-min)))
		  (setq pos (1- pos))
		  (goto-char (1- (point))))
		;; delete linefeed of start point.
		(when (and (eolp) (>= (point-max) (1+ pos)))
		  (setq pos (1+ pos)))
		pos))))

(defun lua-block-line-end-position (pos)
  (when pos
    (save-excursion
      (goto-char pos)
      (let ((xor '(lambda (a b) (and (or a b) (not (and a b)))))
			(pos (point)))
		(while (and (not (funcall xor (eobp) (eolp)))
					(>= (point-max) pos))
		  (setq pos (1+ pos))
		  (goto-char (1+ (point))))
		pos))))

(defun lua-block-function ()
  "Point position's word decides behavior."
  (let* ((cur (current-word))
		 ;; if point after END, dec point and get face
		 (p (point))
		 (face (get-text-property p 'face))
		 (p (if (and (eq nil face) (> p 3))
				(1- p)
			  p)))
    (when (and (member cur '("else" "elseif" "end"))
			   (eq face 'font-lock-keyword-face))
      (let* ((pos (lua-block-corresponding-position p))
			 (sp (lua-block-line-beginning-position pos))
			 (ep (lua-block-line-end-position pos)))
		(when pos
		  ;; display line contents to minibuffer
		  (when (memq lua-block-highlight-toggle '(t minibuffer))
			(message "%d: %s"
					 (1+ (count-lines (point-min) sp)) (buffer-substring sp ep)))
		  ;; do overlay
		  (when (memq lua-block-highlight-toggle '(t overlay))
			(lua-block-do-highlight sp ep)))))))

(defun lua-block-stmt-if (pos)
  (save-excursion
    (goto-char pos)
    (let ((status 'skip))
      (while (and (not (bolp))
				  (eq status 'skip))
		(forward-char -1)
		(let ((ch (char-after)))
		  (cond
		   ((memq ch '(?\n ?\r ?\())
			(setq status t))
		   ((memq ch '(32 \t))
			(setq status 'skip))
		   (t
			(setq status nil)))))
      (when (eq status 'skip)
		(setq status t))
      status)))

(defun lua-block-corresponding-position (pos)
  "Get point of corresponding line."
  (save-excursion
    (goto-char pos)
    (let ((key 1) pos face cur)
      (while (and (> key 0)
				  (re-search-backward lua-block-keyword-regex nil t))
		(setq pos (match-beginning 1)
			  face (get-text-property pos 'face)
			  cur (current-word))
		(when (and (eq face 'font-lock-keyword-face)
				   (not (string= cur "elseif"))
				   (member cur lua-block-keyword-list)
				   ;; case: STMT if (or unless, while, until) EXPR
				   (cond
					((member cur '("if" "while"))
					 (lua-block-stmt-if pos))
					(t t)))
		  (cond
		   ((and (string= cur "end"))
			(setq key (1+ key)))
		   ((not (string= cur "while")) ; 'while' and 'do' combine into one statement
			(setq key (1- key))))))
      (when (= key 0)
		pos))))

(defun lua-block-do-highlight (beg end)
  "Do overlay corresponding line."
  (if lua-block-highlight-overlay
      (move-overlay lua-block-highlight-overlay beg end)
    (setq lua-block-highlight-overlay (make-overlay beg end)))
  (overlay-put lua-block-highlight-overlay
			   'face lua-block-highlight-face)
  (add-hook 'pre-command-hook 'lua-block-highlight-done))

(defun lua-block-highlight-done ()
  "After do overlay, restore the line to original color."
  (remove-hook 'pre-command-hook 'lua-block-highlight-done)
  (if lua-block-highlight-overlay
      (delete-overlay lua-block-highlight-overlay)))

(defun lua-block-highlight-toggle ()
  "Switch on/off for lua-block-mode."
  (interactive)
  (if lua-block-highlight-toggle
      (setq lua-block-highlight-toggle nil)
    (setq lua-block-highlight-toggle t)))

(provide 'lua-block)

;; Local Variables:
;; Coding: utf-8
;; End:

;;; lua-block.el ends here
