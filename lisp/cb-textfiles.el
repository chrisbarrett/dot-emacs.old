;;; cb-textfiles.el --- Config for common plaintext file formats.

;; Copyright (C) 2013 Chris Barrett

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

;; Config for common plaintext file formats.

;;; Code:

(require 'use-package)

(use-package csv-mode
  :ensure t
  :commands (csv-mode)
  :mode  ("\\.csv$" . csv-mode)
  :init
  ;; Override broken var
  (defconst csv-mode-line-help-echo ""))

(provide 'cb-textfiles)

;;; cb-textfiles.el ends here
