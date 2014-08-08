;;; config-helm.el --- Configuration for helm

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

;; Configuration for helm

;;; Code:

(require 'utils-common)
(require 'config-evil)

(cb:install-package 'helm t)
(cb:install-package 'helm-projectile)

(custom-set-variables
 '(helm-adaptive-history-file (f-join cb:tmp-dir "helm-adaptive-history"))
 '(helm-ff-skip-boring-files t)
 '(helm-boring-file-regexp-list '("\\.DS_Store" "\\.elc$")))

(custom-set-faces
 '(helm-selection
   ((((background light)) :background "white" :foreground "black")
    (((background dark))  :background "black" :foreground "white")
    ((t :underline nil)))))

;;; Commands

(defun cb-helm:ff-tilde ()
  (interactive)
  (if (looking-back "/")
      (helm-insert-in-minibuffer "~/" t)
    (call-interactively 'self-insert-command)))

;;; Key bindings

(after 'helm-files
  (define-key helm-find-files-map (kbd "~") 'cb-helm:ff-tilde))

(provide 'config-helm)

;;; config-helm.el ends here
