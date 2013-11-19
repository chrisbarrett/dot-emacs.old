;;; cb-tex.el --- Configuration for TeX

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130917.1426

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

;; Configuration for TeX

;;; Code:

(require 'use-package)
(require 'cb-lib)

(unless (package-installed-p 'auctex)
  (package-install 'auctex))

;; Configure smartparens for latex.
(after 'smartparens
  (after 'tex
    (use-package smartparens-latex)))

;; `tex' is part of the auctex package and provides features like in-buffer
;; formula previews and a rich editing commandset.
(use-package tex
  :defer t
  :commands (TeX-mode LaTeX-mode)
  :mode ((".\\lco$" . LaTeX-mode)
         (".\\tex$" . TeX-mode))
  :config
  (progn
    (use-package preview)
    (use-package latex)

    (TeX-global-PDF-mode +1)

    (defadvice TeX-complete-symbol (after position-point activate)
      "Position point inside braces."
      (when (equal (char-before) ?\})
        (forward-char -1)))

    (setq TeX-auto-save t
          TeX-parse-self t)

    (after 'tex
      (bind-keys
        :map TeX-mode-map
        "M-P" 'flycheck-previous-error
        "M-N" 'flycheck-next-error
        "TAB" 'TeX-complete-symbol))))

;; `tex-fold' is part of auctex and provides improved folding commands over the
;; defaults provided by tex-mode.
(use-package tex-fold
  :defer t
  :init
  (hook-fns '(tex-mode-hook latex-mode-hook)
    (TeX-fold-mode +1))
  :config
  (after 'evil
    (evil-define-key 'normal TeX-mode-map
      (kbd "z m") 'TeX-fold-buffer
      (kbd "z r") 'TeX-fold-clearout-buffer
      (kbd "SPC") 'TeX-fold-dwim)))

;; `whizzytex' provides live output display with incremental compilation.
(use-package whizzytex
  :defer t
  :commands whizzytex-mode
  :init
  (progn

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
      (whizzytex-mode +1))))

(provide 'cb-tex)

;; Local Variables:
;; End:

;;; cb-tex.el ends here
