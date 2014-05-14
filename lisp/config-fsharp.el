;;; config-fsharp.el --- Configure fsharp

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configure fsharp

;;; Code:

(require 'utils-common)
(require 'super-smart-ops)
(require 'config-ocaml)

(cb:declare-package-installer fsharp
  :match (rx ".fs" (? (any "i" "y" "l" "x")) eol)
  :packages (fsharp-mode))

(sp-with-modes '(fsharp-mode)
  (sp-local-pair "[|" "|]")
  (sp-local-pair "[<" ">]")
  (sp-local-pair "`" nil :actions nil))

(sp-with-modes '(fsharp-mode)
  (sp-local-pair "\"" "\"" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "{" "}" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "[" "]" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "(" ")" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "'" nil :actions nil))

(hook-fns '(fsharp-mode)
  (add-hook 'post-self-insert-hook 'cb-ocaml:maybe-pad-parens nil t))

(put 'fsharp-mode 'smart-operator-alist cb-ocaml:smart-operator-list)

(after 'fsharp-mode
  (cb-ocaml:set-keys fsharp-mode-map))

(after 'fsharp-mode
  (define-key fsharp-mode-map (kbd "M-RET") 'cb-ocaml:meta-ret))


(provide 'config-fsharp)

;;; config-fsharp.el ends here
