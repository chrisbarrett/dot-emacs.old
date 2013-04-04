;;; cb-foundation --- Basic configuration

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Basic configuration required for a sane editing environment.

;;; Code:

;;; Disable vc modes.
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(eval-after-load "vc"
  '(remove-hook 'find-file-hooks 'vc-find-file-hook))

;;; Use unique buffer names based on file path.
(eval-after-load 'uniquify
  '(setq uniquify-buffer-name-style 'forward
         uniquify-separator "/"
         uniquify-after-kill-buffer-p t
         uniquify-ignore-buffers-re "^\\*"))

(auto-compression-mode +1)
(setq
 redisplay-dont-pause         t
 column-number-mode           t
 echo-keystrokes              0.02
 inhibit-startup-message      t
 transient-mark-mode          t
 shift-select-mode            nil
 require-final-newline        t
 delete-by-moving-to-trash    nil
 initial-major-mode           'emacs-lisp-mode
 initial-scratch-message      nil
 x-select-enable-clipboard    t
 font-lock-maximum-decoration t
 ring-bell-function           'ignore
 initial-scratch-message      nil
 truncate-partial-width-windows     nil
 confirm-nonexistent-file-or-buffer nil
 )
(setq-default
 indent-tabs-mode             nil
 fill-column                  80)

;;; Encodings
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

;;; File-handling
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Suppress \"Active processes exist\" query when exiting Emacs."
  (flet ((process-list ()))
    ad-do-it))

(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "Trim whitespace on `kill-line'."
  (unless (bolp)
    (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

(defadvice whitespace-cleanup (around whitespace-cleanup-indent-tab activate)
  "Fix `whitespace-cleanup' bug when using `indent-tabs-mode'."
  (let ((whitespace-indent-tabs-mode indent-tabs-mode)
        (whitespace-tab-width tab-width))
    ad-do-it))

;;; Buffers

(defvar cb:kill-buffer-ignored-list
  '("*scratch*" "*Messages*" "*GROUP*" "*shell*" "*eshell*"))

(defun cb:kill-current-buffer ()
  "Kill the current buffer.
If this buffer is a member of `kill-buffer-ignored-list, bury it rather than killing it."
  (interactive)
  (if (member (buffer-name (current-buffer)) cb:kill-buffer-ignored-list)
      (bury-buffer)
    (kill-buffer (current-buffer))))

(global-set-key (kbd "C-x C-K") 'cb:kill-current-buffer)

(defun sudo-edit (&optional arg)
  "Edit a file with elevated privileges."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun cb:hide-dos-eol ()
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M [])
  (aset buffer-display-table ?\^L []))

(add-hook 'find-file-hook 'cb:hide-dos-eol)

(provide 'cb-foundation)

;;; cb-foundation.el ends here
