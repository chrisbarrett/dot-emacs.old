;;; config-compilation.el --- Configuration for compile-mode

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

;; Configuration for compile-mode

;;; Code:

(require 'utils-common)

(custom-set-variables
 '(compilation-window-height 12)
 '(compilation-scroll-output 'first-error))

;;; Colourise output.

(defun cb:ansi-colourise-compilation ()
  (ansi-color-apply-on-region compilation-filter-start (point)))

(add-hook 'compilation-filter-hook 'cb:ansi-colourise-compilation)

;;; Automatically close compilation buffers.

(defun cb:compile-autoclose (buf string)
  "Automatically close the compile window."
  (cond
   ;; Ignore if this isn't a normal compilation window.
   ((not (equal (buffer-name buf) "*compilation*")))

   ((not (s-contains? "finished" string))
    (message "Compilation exited abnormally: %s" string))

   ((s-contains? "warning" (with-current-buffer buf
                             (buffer-string)) 'ignore-case)
    (message "Compilation succeeded with warnings"))

   (t
    (ignore-errors
      (delete-window (get-buffer-window buf)))
    (message "Compilation succeeded"))))

(add-to-list 'compilation-finish-functions 'cb:compile-autoclose)

;;; Key bindings

(bind-key "C-c b" 'compile)
(bind-key "C-c C-b" 'recompile)

(provide 'config-compilation)

;;; config-compilation.el ends here
