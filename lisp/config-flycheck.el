;;; config-flycheck.el --- Configuration for flycheck

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris@chris-ubuntu>
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

;; Configuration for flycheck

;;; Code:

(require 'utils-common)

(cb:install-package 'flycheck)
(global-flycheck-mode)

(custom-set-variables
 '(flycheck-disabled-checkers '(coq)))

(cb:install-package 'flycheck-cask)
(add-hook 'flycheck-mode-hook 'flycheck-cask-setup)

(add-to-list 'safe-local-eval-forms '(flycheck-cask-setup))

;;; Key bindings

(define-key flycheck-mode-map (kbd "M-N") 'flycheck-next-error)
(define-key flycheck-mode-map (kbd "M-P") 'flycheck-previous-error)

(provide 'config-flycheck)

;;; config-flycheck.el ends here
