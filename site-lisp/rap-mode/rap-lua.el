;;; rap-lldb.el --- REPL Adapter Protocol mode for Lua      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ruin0x11

;; Author: ipickering2@gmail.com <ipickering2@gmail.com>
;; Keywords: languages

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

;; URL: https://github.com/Ruin0x11/rap-mode
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "4.0"))
;; Version: 0.2

;;; Commentary:
;; rap-mode adapter for Lua

;;; Code:

(require 'rap-mode)

(defun rap-lua--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (rap--put-if-absent :host "localhost")
      (rap--put-if-absent :type "lua")
      (rap--put-if-absent :port 4567)
      (rap--put-if-absent :name "Lua REPL")))

(rap-register-repl-provider "lua" 'rap-lua--populate-start-file-args)

(rap-register-debug-template "lua"
                             (list :type "lua"
                                   :cwd nil
                                   :request nil
                                   :file nil
                                   :reAttach t
                                   :program nil
                                   :name "Lua::Run"))

(provide 'rap-lua)
;;; rap-lua.el ends here
