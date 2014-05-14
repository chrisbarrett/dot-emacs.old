;;; config-tex.el --- Configure tex and latex

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

;; Configure tex and latex

;;; Code:

(require 'utils-common)

(cb:declare-package-installer tex
  :match (rx "." (or "tex" "dtx" "ins" "ltx" "sty"
                     "cls" "clo" "bbl" "dry" "lco") eol)
  :packages (auctex latex-preview-pane))

(add-to-list 'auto-mode-alist '("\\.lco$" . latex-mode))

(custom-set-variables
 '(TeX-auto-save t)
 '(TeX-parse-self t)
 '(TeX-master nil)
 '(TeX-PDF-mode t))

(after 'tex

  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook (lambda () (abbrev-mode +1)))

  (add-to-list 'face-remapping-alist '(bad-face . flycheck-error))

  (latex-preview-pane-enable)

  (defadvice TeX-complete-symbol (after position-point activate)
    "Position point inside braces."
    (when (equal (char-before) ?\})
      (forward-char -1)))

  (require 'whizzytex)

  (defvar whizzytex-sty-installation "/usr/local/share/whizzytex/latex/whizzytex.sty"
    "Path to the whizzytex macro package.")

  (defvar whizzytex-src (f-join cb:lib-dir "whizzytex" "src")
    "Path to the whizzytex sources.")

  (defvar whizzy-command-name (f-join whizzytex-src "whizzytex"))

  (defun cbwh:install-tex-macros ()
    "Prompt the user to install the tex macros if they do not exist."
    (unless (f-exists? whizzytex-sty-installation)
      (when (y-or-n-p (format "Install whizzytex macros into %s? "
                              (f-dirname whizzytex-sty-installation)))
        ;; Make installation directory and copy package there.
        (%-sudo (%-sh "mkdir -p" (f-dirname whizzytex-sty-installation)))
        (%-sudo (%-sh "cp -f"
                      (%-quote (f-join whizzytex-src "whizzytex.sty"))
                      (%-quote whizzytex-sty-installation))))))

  (hook-fn 'tex-mode-hook
    (cbwh:install-tex-macros)
    (whizzytex-mode +1))

  (TeX-global-PDF-mode +1)

  (require 'preview)
  (require 'latex)

  (after 'flycheck
    (bind-keys
      :map TeX-mode-map
      "M-P" 'flycheck-previous-error
      "M-N" 'flycheck-next-error
      "TAB" 'TeX-complete-symbol))

  (autoload 'TeX-fold-mode "tex-fold")
  (hook-fns '(tex-mode-hook latex-mode-hook)
    (TeX-fold-mode +1))

  (after '(evil tex)
    (evil-define-key 'normal TeX-mode-map
      (kbd "z m") 'TeX-fold-buffer
      (kbd "z r") 'TeX-fold-clearout-buffer
      (kbd "SPC") 'TeX-fold-dwim))

  )

(provide 'config-tex)

;;; config-tex.el ends here
