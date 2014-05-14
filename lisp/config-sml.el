;;; config-sml.el --- Configure SML

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

;; Configure SML

;;; Code:

(require 'utils-common)

(cb:declare-package-installer standard-ml
  :match (rx "." (or "sml" "sig" "cm" "grm"))
  :packages (sml-mode))

(custom-set-variables
 '(sml-indent-level 2))

(add-to-list 'completion-ignored-extensions "\\.cm")
(add-to-list 'completion-ignored-extensions "\\.CM")

(provide 'config-sml)

;;; config-sml.el ends here
