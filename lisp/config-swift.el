;;; config-swift.el --- Configuration for swift-mode.

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

;; Configuration for swift-mode.

;;; Code:

(require 'utils-common)
(require 'super-smart-ops)
(require 'config-smartparens)

;; Use development version sources when available, otherwise install from MELPA.
(or (require 'swift-mode (f-join user-home-directory "Projects" "swift-mode") t)
    (cb:install-package 'swift-mode))

;;; Smart ops


(defun cb-swift:smart-op-insert-no-leading-space (op)
  "Insert OP without any preceding padding."
  (smart-op-insert op)
  (save-excursion
    (search-backward op)
    (unless (s-matches? (rx bol (* space) eol)
                        (buffer-substring (line-beginning-position) (point)))
      (delete-horizontal-space))))

(defun cb-swift:smart-colon ()
  "Insert a colon with a trailing space."
  (interactive "*")
  (cb-swift:smart-op-insert-no-leading-space ":"))

(defun cb-swift:smart-comma ()
  "Insert a comma with a trailing space."
  (interactive "*")
  (cb-swift:smart-op-insert-no-leading-space ","))

(declare-smart-ops 'swift-mode
  :custom
  '((":" . cb-swift:smart-colon)
    ("," . cb-swift:smart-comma)))


;;; Smartparens


(sp-with-modes '(swift-mode)
  (sp-local-pair "{" "}" :post-handlers '(:add sp-generic-leading-space)))


(provide 'config-swift)

;;; config-swift.el ends here
