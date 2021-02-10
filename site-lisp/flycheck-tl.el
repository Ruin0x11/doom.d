;;; flycheck-tl.el --- Flycheck checker for tl       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ruin0x11

;; Author: Ruin0x11 <ipickering2@gmail.com>
;; Keywords: processes

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

;;; Commentary:

;;

;;; Code:
(require 'flycheck)

(defun flycheck-tl--working-directory (&rest _ignored)
  "Find directory with from which we can run tl."
  (and buffer-file-name
       (locate-dominating-file buffer-file-name "types/")
       (locate-dominating-file buffer-file-name ".luacheckrc")))

(flycheck-def-option-var flycheck-lua-tl-include nil lua-tl
  "Include directories."
  :type 'list
  :safe #'listp)

(flycheck-def-option-var flycheck-lua-tl-load nil lua-tl
  "Load file."
  :type 'string
  :safe #'stringp)

(flycheck-define-checker lua-tl
  "Lua tl checker."
  :command ("tl"
            "check"
            (option-list "-I" flycheck-lua-tl-include)
            (option "-l" flycheck-lua-tl-load)
            source-original)
  :predicate flycheck-tl--working-directory
  :working-directory flycheck-tl--working-directory
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes lua-mode)

;;;###autoload
(defun flycheck-tl-setup ()
  "Setup flycheck-tl.
Add `lua-tl' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'lua-tl t))

(provide 'flycheck-tl)
;;; flycheck-tl.el ends here
