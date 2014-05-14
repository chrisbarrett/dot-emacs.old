;;; config-org-reveal.el --- Configure org-reveal

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

;; Configure org-reveal

;;; Code:

(require 'utils-common)
(autoload 'popup-menu* "popup")

(cb:install-package 'ox-reveal t)

(custom-set-variables
 '(org-reveal-root (concat "file://" (f-join cb:lib-dir "reveal.js"))))

(defun cb-org:reveal-read-transition ()
  (popup-menu*
   (-map 'popup-make-item
         '("Cube" "Page" "Concave" "Zoom" "Linear" "Fade" "None" "Default"))
   :isearch t))

(defun cb-org:reveal-read-theme ()
  (popup-menu*
   (-map 'popup-make-item
         '("Default" "Sky" "Beige" "Simple" "Serif" "Night Moon" "Simple" "Solarized"))
   :isearch t))

(defun cb-org:reveal-read-frag-style ()
  (popup-menu*
   (-map 'popup-make-item
         '("grow" "shrink" "roll-in" "fade-out"
           "highlight-red" "highlight-green" "highlight-blue"))
   :isearch t))

(provide 'config-org-reveal)

;;; config-org-reveal.el ends here
