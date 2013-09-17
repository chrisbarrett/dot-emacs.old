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

(unless (package-installed-p 'auctex)
  (package-install 'auctex))

(use-package preview
  :defer t
  :init
  (hook-fns '(tex-mode-hook latex-mode-hook)
    (require 'preview)))

(use-package latex
  :defer t
  :init
  (hook-fn 'tex-mode-hook
    (require 'latex)))

(use-package tex
  :defer t
  :config
  (progn
    (defadvice tex-mode (after run-hooks activate)
      (run-hooks 'tex-mode-hook))

    (setq TeX-auto-save t
          TeX-parse-self t)

    (hook-fns '(tex-mode-hook latex-mode-hook)
      (local-set-key (kbd "M-P") 'previous-error)
      (local-set-key (kbd "M-N") 'next-error)
      (local-set-key (kbd "TAB") 'TeX-complete-symbol))))

(use-package tex-fold
  :defer t
  :init
  (hook-fns '(tex-mode-hook latex-mode-hook)
    (TeX-fold-mode +1)
    (when (featurep 'evil)
      (evil-local-set-key 'normal (kbd "z m") 'TeX-fold-buffer)
      (evil-local-set-key 'normal (kbd "z r") 'TeX-fold-clearout-buffer)
      (evil-local-set-key 'normal (kbd "SPC") 'TeX-fold-dwim))))

(provide 'cb-tex)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-tex.el ends here
