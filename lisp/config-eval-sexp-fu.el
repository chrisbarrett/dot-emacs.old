;;; config-eval-sexp-fu.el --- Configure eval-sexp-fu

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

;; Configure eval-sexp-fu

;;; Code:

(require 'utils-common)
(require 'config-theme)

(cb:install-package 'eval-sexp-fu t)

(custom-set-variables
 '(eval-sexp-fu-flash-duration 0.2))

(custom-set-faces
 `(eval-sexp-fu-flash-error
   ((t (:background ,solarized-hl-orange)))))

(add-hook 'cb:lisp-modes-hook 'turn-on-eval-sexp-fu-flash-mode)

(add-to-list 'face-remapping-alist '(eval-sexp-fu-flash . intense-flash))

(provide 'config-eval-sexp-fu)

;;; config-eval-sexp-fu.el ends here
