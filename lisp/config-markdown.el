;;; config-markdown.el --- Configure markdown

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

;; Configure markdown

;;; Code:

(require 'utils-common)

(cb:declare-package-installer markdown
  :match (rx "." (or "md" "markdown") eol)
  :packages (markdown-mode
             markdown-toc))

(add-to-list 'auto-mode-alist (cons (rx "." (or "md" "markdown") eol) 'markdown-mode))

(custom-set-faces
 '(markdown-header-face-1 ((t :height 1.3)))
 '(markdown-header-face-2 ((t :height 1.1))))

(put 'markdown-mode 'imenu-generic-expression
     '(("title"  "^\\(.*\\)[\n]=+$" 1)
       ("h2-"    "^\\(.*\\)[\n]-+$" 1)
       ("h1"   "^# \\(.*\\)$" 1)
       ("h2"   "^## \\(.*\\)$" 1)
       ("h3"   "^### \\(.*\\)$" 1)
       ("h4"   "^#### \\(.*\\)$" 1)
       ("h5"   "^##### \\(.*\\)$" 1)
       ("h6"   "^###### \\(.*\\)$" 1)
       ("fn"   "^\\[\\^\\(.*\\)\\]" 1)))

(provide 'config-markdown)

;;; config-markdown.el ends here
