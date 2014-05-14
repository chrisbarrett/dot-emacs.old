;;; config-org-mime.el --- Configure org-mime

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

;; Configure org-mime

;;; Code:

(require 'utils-common)
(require 'org-mime)
(require 'org)

(custom-set-variables
 '(org-mime-default-header "#+OPTIONS: num:nil toc:nil latex:t\n"))

(define-key org-mode-map (kbd "C-c M-o") 'org-mime-org-buffer-htmlize)

(after 'message
  (define-key message-mode-map  (kbd "C-c M-o") 'org-mime-htmlize))

(hook-fn 'org-mime-html-hook
  (org-mime-change-element-style
   "blockquote" "border-left: 2px solid #B0B0B0; padding-left: 4px;")
  (org-mime-change-element-style
   "pre" "border-left: 2px solid #B0B0B0; padding-left: 4px;"))

(defun org-export-grab-title-from-buffer ()
  "")

(provide 'config-org-mime)

;;; config-org-mime.el ends here
