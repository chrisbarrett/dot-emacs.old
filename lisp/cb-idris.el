;;; cb-idris.el --- Configuration for the Idris language.

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

;; Configuration for the Idris language.

;;; Code:

(require 'use-package)
(require 'cb-lib)

;; `idris-mode' provides editing support for the Idris language.
(use-package idris-mode
  :mode (("\\.idr$" . idris-mode))
  :config
  (after 'idris-mode
    (define-key idris-mode-map (kbd "C-c C-z") 'idris-switch-to-output-buffer)))

;; Define a command to switch from the repl to the last Idris src buffer.

(defun idris-switch-to-src ()
  "Pop to the last idris source buffer."
  (interactive)
  (-if-let (buf (car (--filter-buffers (derived-mode-p 'idris-mode))))
    (pop-to-buffer buf)
    (error "No idris buffers")))

(after 'idris-repl
  (define-key idris-repl-mode-map (kbd "C-c C-z") 'idris-switch-to-src))

(provide 'cb-idris)

;;; cb-idris.el ends here
