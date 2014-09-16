;;; config-term.el --- Configuration for terminals.

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

;; Configuration for terminals.

;;; Code:

(require 'utils-common)

(defun cb:term-cycle (&optional project)
  "Cycle through various terminal window states."
  (interactive (list (format "*%s*" (f-filename shell-file-name))))
  (cond
   ;; If terminal is maximized, restore previous window config.
   ((and (derived-mode-p 'term-mode)
         (equal 1 (length (window-list))))
    (or (ignore-errors (jump-to-register :term-fullscreen) t)
        (bury-buffer)))

   ;; If we're looking at the terminal, maximise it.
   ((derived-mode-p 'term-mode)
    (delete-other-windows))

   (project
    (-if-let (bufs (--filter-buffers (and (derived-mode-p 'term-mode)
                                          (s-contains? project default-directory))))
        (switch-to-buffer (car bufs))
      (ansi-term shell-file-name (format "ansi-term: %s" project))))

   ((--none? (with-current-buffer it (derived-mode-p 'term-mode))
             (buffer-list))
    (ansi-term shell-file-name))

   ;; Otherwise show the terminal.
   (t
    ;; Hide the term window if it's visible.
    (-when-let (win (--first
                     (with-current-buffer (window-buffer it)
                       (derived-mode-p 'term-mode))
                     (window-list)))
      (delete-window win))
    ;; Save this configuration to a register so that it can be restored
    ;; for later positions in the cycle.
    (window-configuration-to-register :term-fullscreen)
    ;; Show terminal.
    (switch-to-buffer (--first-buffer (derived-mode-p 'term-mode))))))

(add-hook 'term-mode-hook (lambda () (setq yas-dont-activate t)))

(provide 'config-term)

;;; config-term.el ends here
