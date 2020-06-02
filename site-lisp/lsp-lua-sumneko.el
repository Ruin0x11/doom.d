;;; lsp-lua-sumneko.el --- LSP client for sumneko/lua-language-server  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ruin0x11

;; Author: Ruin0x11
;; Keywords: tools

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


(defgroup lsp-lua-sumneko nil
  "LSP support for sumneko-lua."
  :group 'lsp-mode
  :link '(url-link "https://github.com/sumneko/lua-lsp-server"))

(defcustom lsp-clients-sumneko-lua-path (expand-file-name "~/build/lua-language-server/bin/Windows/lua-language-server.exe")
  "Path to java which will be used for running sumneko-lua language server."
  :group 'lsp-lua-sumneko
  :risky t
  :type 'file)

(defcustom lsp-clients-sumneko-lua-main-path (expand-file-name "~/build/lua-language-server/main.lua")
  "Path to jar which will be used for running SumnekoLua language server."
  :group 'lsp-lua-sumneko
  :risky t
  :type 'file)


;;; Extra configuration

(defcustom lsp-lua-sumneko-color-mode "Semantic" nil
  :group 'lsp-lua-sumneko
  :type '(choice (:tag "Grammar" "Semantic")))

(defcustom lsp-lua-sumneko-completion-call-snippet "Disable" nil
  :group 'lsp-lua-sumneko
  :type '(choice (:tag "Disable" "Both" "Replace")))

(defcustom lsp-lua-sumneko-completion-enable t nil
  :group 'lsp-lua-sumneko
  :type 'boolean)

(defcustom lsp-lua-sumneko-completion-keyword-snippet "Replace" nil
  :group 'lsp-lua-sumneko
  :type '(choice (:tag "Disable" "Both" "Replace")))

(defcustom lsp-lua-sumneko-develop-debugger-port 11412 nil
  :group 'lsp-lua-sumneko
  :type 'number)

(defcustom lsp-lua-sumneko-develop-debugger-wait nil nil
  :group 'lsp-lua-sumneko
  :type 'boolean)

(defcustom lsp-lua-sumneko-develop-enable nil nil
  :group 'lsp-lua-sumneko
  :type 'boolean)

(defcustom lsp-lua-sumneko-diagnostics-disable nil nil
  :group 'lsp-lua-sumneko
  :type 'lsp-string-vector)

(defcustom lsp-lua-sumneko-diagnostics-enable t nil
  :group 'lsp-lua-sumneko
  :type 'boolean)

(defcustom lsp-lua-sumneko-diagnostics-globals nil nil
  :group 'lsp-lua-sumneko
  :type 'lsp-string-vector)

(defcustom lsp-lua-sumneko-diagnostics-severity nil nil
  :group 'lsp-lua-sumneko
  :type 'nil)

(defcustom lsp-lua-sumneko-runtime-path ["?.lua" "?/init.lua" "?/?.lua"] nil
  :group 'lsp-lua-sumneko
  :type 'lsp-string-vector)

(defcustom lsp-lua-sumneko-runtime-version "Lua 5.3" nil
  :group 'lsp-lua-sumneko
  :type '(choice (:tag "Lua 5.1" "Lua 5.2" "Lua 5.3" "Lua 5.4" "LuaJIT")))

(defcustom lsp-lua-sumneko-workspace-ignore-dir [".vscode"] nil
  :group 'lsp-lua-sumneko
  :type 'lsp-string-vector)

(defcustom lsp-lua-sumneko-workspace-ignore-submodules t nil
  :group 'lsp-lua-sumneko
  :type 'boolean)

(defcustom lsp-lua-sumneko-workspace-library nil nil
  :group 'lsp-lua-sumneko
  :type 'nil)

(defcustom lsp-lua-sumneko-workspace-max-preload 300 nil
  :group 'lsp-lua-sumneko
  :type 'number)

(defcustom lsp-lua-sumneko-workspace-preload-file-size 100 nil
  :group 'lsp-lua-sumneko
  :type 'number)

(defcustom lsp-lua-sumneko-workspace-use-git-ignore t nil
  :group 'lsp-lua-sumneko
  :type 'boolean)


(defvar lsp-lua-sumneko-default-settings '(("Lua.workspace.useGitIgnore" lsp-lua-sumneko-workspace-use-git-ignore t)
                                           ("Lua.workspace.preloadFileSize" lsp-lua-sumneko-workspace-preload-file-size)
                                           ("Lua.workspace.maxPreload" lsp-lua-sumneko-workspace-max-preload)
                                           ("Lua.workspace.library" lsp-lua-sumneko-workspace-library)
                                           ("Lua.workspace.ignoreSubmodules" lsp-lua-sumneko-workspace-ignore-submodules t)
                                           ("Lua.workspace.ignoreDir" lsp-lua-sumneko-workspace-ignore-dir)
                                           ("Lua.runtime.version" lsp-lua-sumneko-runtime-version)
                                           ("Lua.runtime.path" lsp-lua-sumneko-runtime-path)
                                           ("Lua.diagnostics.severity" lsp-lua-sumneko-diagnostics-severity)
                                           ("Lua.diagnostics.globals" lsp-lua-sumneko-diagnostics-globals)
                                           ("Lua.diagnostics.enable" lsp-lua-sumneko-diagnostics-enable t)
                                           ("Lua.diagnostics.disable" lsp-lua-sumneko-diagnostics-disable)
                                           ("Lua.develop.enable" lsp-lua-sumneko-develop-enable t)
                                           ("Lua.develop.debuggerWait" lsp-lua-sumneko-develop-debugger-wait t)
                                           ("Lua.develop.debuggerPort" lsp-lua-sumneko-develop-debugger-port)
                                           ("Lua.completion.keywordSnippet" lsp-lua-sumneko-completion-keyword-snippet)
                                           ("Lua.completion.enable" lsp-lua-sumneko-completion-enable t)
                                           ("Lua.completion.callSnippet" lsp-lua-sumneko-completion-call-snippet)
                                           ("Lua.color.mode" lsp-lua-sumneko-color-mode)))

(lsp-register-custom-settings lsp-lua-sumneko-default-settings)


;;; LSP client

(defun lsp-clients-sumneko-lua--create-connection ()
  "Create connection to sumneko lua language server."
  (lsp-stdio-connection
   (lambda ()
     (list lsp-clients-sumneko-lua-path lsp-clients-sumneko-lua-main-path))
   (lambda ()
     (f-exists? lsp-clients-sumneko-lua-path))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-clients-sumneko-lua--create-connection)
                  :major-modes '(lua-mode)
                  :priority 1
                  :multi-root t
                  :server-id 'sumneko-lua))

(provide 'lsp-lua-sumneko)
;;; lsp-lua-sumneko.el ends here
