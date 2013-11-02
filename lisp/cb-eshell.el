;;; cb-eshell.el --- Configuration for eshell

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130915.1146

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

;; Configuration for eshell

;;; Code:

(require 'use-package)
(require 'cb-lib)

;; `eshell' is a terminal emulator written in elisp.
(use-package eshell
  :commands eshell
  :bind ("<f1>" . cb:term-cycle)
  :config
  (progn

    ;; Case-insensitive filename completion.
    (setq eshell-cmpl-ignore-case t)

    (defun cb:term-cycle (&optional arg)
      "Cycle through various terminal window states."
      (interactive)
      (cond

       ;; If terminal is maximized, restore previous window config.
       ((and (derived-mode-p 'eshell-mode)
             (equal 1 (length (window-list)))
             (equal (buffer-name) "*eshell*"))
        (or (ignore-errors (jump-to-register :term-fullscreen) t)
            (bury-buffer)))

       ;; If we're looking at the terminal, maximise it.
       ((derived-mode-p 'eshell-mode)
        (delete-other-windows))

       ;; Otherwise show the terminal.
       (t
        ;; Hide the term window if it's visible.
        (-when-let (win (--first (equal (buffer-name (window-buffer it))
                                        "*eshell*")
                                 (window-list)))
          (delete-window win))
        ;; Save this configuration to a register so that it can be restored
        ;; for later positions in the cycle.
        (window-configuration-to-register :term-fullscreen)
        ;; Show terminal.
        (eshell arg))))

    (defun eshell/clear ()
      "Clear the eshell buffer."
      (interactive)
      (let ((inhibit-read-only t))
        ;; Delete all but the last line of prompt.
        (delete-region (point-min)
                       (save-excursion
                         (goto-char (point-max))
                         (search-backward-regexp (rx bol space (or "#" "%") space) nil t)
                         (point)))))

    (defun eshell-subshelling-ret ()
      (interactive)
      (end-of-line)
      (unless (s-matches? (rx "&" (* space)) (current-line))
        (just-one-space)
        (insert "&")
        (eshell-send-input)))

    (hook-fn 'eshell-mode-hook
      (local-set-key (kbd "M->") 'cb:append-buffer)
      (local-set-key (kbd "C-l") 'eshell/clear)
      (local-set-key (kbd "C-<return>") 'eshell-subshelling-ret))

    ;; Configure the eshell prompt.
    ;;
    ;; Displays the current working directory only when it changes.

    (defun cb-eshell:cwd-info ()
      (concat
       "\n"
       "------------------------- "
       (abbreviate-file-name (eshell/pwd))
       "\n"))

    (defvar cb-eshell:prev-dir nil)

    (setq

     eshell-prompt-regexp
     (rx bol
         (* (not (any "#" "%")))
         (or "#" "%")
         space)

     eshell-prompt-function
     (lambda ()
       (prog1
           (concat
            (unless (equal (eshell/pwd) cb-eshell:prev-dir)
              (cb-eshell:cwd-info))
            (if (= (user-uid) 0) " # " " % "))
         (setq cb-eshell:prev-dir (eshell/pwd)))))))

(provide 'cb-eshell)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-eshell.el ends here
