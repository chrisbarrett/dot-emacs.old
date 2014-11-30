;;; config-paren-face.el --- Configure parenface

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

;; Dim parens in programming languages.

;;; Code:

(require 'utils-common)

(cb:install-package 'paren-face t)

(custom-set-faces
 '(parenthesis
   ((((background light)) :foreground "grey80")
    (((background dark))  :foreground "#505070"))))

(add-to-list 'paren-face-modes 'haskell-mode)
(add-to-list 'paren-face-modes 'inferior-haskell-mode)
(add-to-list 'paren-face-modes 'idris-mode)
(add-to-list 'paren-face-modes 'coq-mode)

(global-paren-face-mode)

(provide 'config-parenface)

;;; config-paren-face.el ends here
