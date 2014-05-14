;;; config-hl-line.el --- Configure hl-line

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

;; Configure hl-line

;;; Code:

(require 'hl-line)
(require 'utils-common)

(when (display-graphic-p)
  (global-hl-line-mode t))

(make-variable-buffer-local 'global-hl-line-mode)

(defun hl-line-disable ()
  (setq global-hl-line-mode nil))

(--each '(eshell-mode-hook
          Man-mode-hook
          haskell-mode-hook
          Info-mode-hook)
  (add-hook it 'hl-line-disable))

(provide 'config-hl-line)

;;; config-hl-line.el ends here
