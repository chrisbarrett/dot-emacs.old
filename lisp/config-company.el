;;; config-company.el --- Configuration for company mode

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

;; Configuration for company mode

;;; Code:

(require 'utils-common)

(cb:install-package 'company)

(custom-set-variables
 '(company-idle-delay 0.1)
 '(company-tooltip-limit 10)
 '(company-minimum-prefix-length 3))

(global-company-mode)
(diminish 'company-mode)

;;; Key bindings

(after 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "C-w") nil))

(provide 'config-company)

;;; config-company.el ends here
