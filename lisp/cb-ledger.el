;;; cb-ledger.el --- Supporting configuration for the Ledger accounting tool.

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

;; Supporting configuration for the Ledger accounting tool.

;;; Code:

(require 'cb-lib)
(require 'use-package)

;; `ledger-mode' provides support for ledger files.
(use-package ledger-mode
  :ensure t
  :commands (ledger-mode)
  :mode ("\\.ledger$" . ledger-mode))

(provide 'cb-ledger)

;;; cb-ledger.el ends here
