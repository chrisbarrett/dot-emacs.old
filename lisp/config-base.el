;;; config-base.el --- configure basic variables.

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

;; configure basic variables.

;;; Code:

(require 'utils-common)
(require 'utils-commands)

(when (fboundp 'scroll-bar-mode)   (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)     (tool-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(when (fboundp 'menu-bar-mode)     (menu-bar-mode -1))

(setq gc-cons-threshold (* 1024 1024 20))

(setq custom-file (concat user-emacs-directory "custom.el"))
(add-to-list 'load-path (concat (getenv "HOME") "/Dropbox/"))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "lib"))

(defalias 'make-local-hook 'ignore)

(setq ring-bell-function 'ignore)

(setq inhibit-startup-message t)

(setq initial-scratch-message nil)

(setq-default transient-mark-mode t)

(setq redisplay-dont-pause t
      echo-keystrokes 0.02
      truncate-partial-width-windows nil)

(setq          initial-major-mode 'org-mode)
(setq-default  major-mode         'org-mode)

(setq confirm-nonexistent-file-or-buffer nil)

(setq delete-by-moving-to-trash nil)

(auto-compression-mode +1)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Do not prompt for confirmation for active processes."
  (noflet ((process-list () nil))
    ad-do-it))

(defadvice whitespace-cleanup (around whitespace-cleanup-indent-tab activate)
  (let ((whitespace-indent-tabs-mode indent-tabs-mode)
        (whitespace-tab-width tab-width))
    ad-do-it))

(defadvice comment-indent-new-line (after add-space activate)
  (when (and comment-start
             (thing-at-point-looking-at (regexp-quote comment-start)))
    (unless (or (thing-at-point-looking-at (rx (+ space))))
      (just-one-space))))

(defadvice insert-for-yank (after clean-whitespace)
  (whitespace-cleanup)
  (delete-trailing-whitespace))

(setq require-final-newline t)

(setq sentence-end-double-space nil)

(hook-fn 'find-file-hook
  "Hide DOS EOL chars."
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M [])
  (aset buffer-display-table ?\^L []))

(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)

(add-hook 'org-mode-hook 'auto-fill-mode)

(setq-default fill-column 80)

(setq bookmark-default-file (f-join cb:tmp-dir "bookmarks"))

(setq save-place-file (f-join cb:tmp-dir "saved-places"))
(setq-default save-place t)

(unless noninteractive
  (require 'saveplace)
  (add-hook 'kill-emacs-hook   'save-place-kill-emacs-hook)
  (add-hook 'kill-buffer-hook  'save-place-to-alist)
  (add-hook 'find-file-hook    'save-place-find-file-hook t)
  (add-hook 'server-visit-hook 'save-place-find-file-hook)
  (add-hook 'server-done-hook  'save-place-kill-emacs-hook))

(require 'backup-dir)
(setq auto-save-file-name-transforms `((".*" ,(concat cb:autosaves-dir "\\1") t))
      backup-by-copying        t
      bkup-backup-directory-info `((".*" ,cb:backups-dir ok-create))
      auto-save-list-file-name (concat cb:autosaves-dir "autosave-list")
      delete-old-versions      t
      kept-new-versions        6
      kept-old-versions        2
      version-control          t)

(setq savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval    60
      savehist-file                 (f-join cb:tmp-dir "savehist"))

(condition-case _
    (savehist-mode +1)
  (void-variable
   (delete-file savehist-file)
   (savehist-mode +1)))

(hook-fn 'kill-emacs-hook
  (ignore-errors
    (when (fboundp 'tramp-cleanup-all-buffers)
      (tramp-cleanup-all-buffers))))

(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(setq x-select-enable-clipboard t)

(setq default-input-method "TeX")

(bind-key "C-x C-\\" 'set-input-method)

(bind-key "RET" 'comment-indent-new-line)

(autoload 'toggle-centred-mode "centred-mode")
(bind-key "C-x c" 'toggle-centred-mode)

(bind-key* "S-SPC" 'execute-extended-command)

(bind-key* "C-<backspace>"
           (command (cond ((< 1 (length (window-list)))
                           (kill-current-buffer)
                           (delete-window))
                          (t
                           (kill-current-buffer)))))

(bind-key "C-c e e" 'toggle-debug-on-error)

(setq vc-follow-symlinks t)

(setq vc-handled-backends '(Git))

(autoload 'vc-git-root "vc-git")

(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(setq comint-prompt-read-only t)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(bind-key* "M-/" 'hippie-expand)

(require 'fringe)
(fringe-mode '(2 . 0))

(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)

(defadvice display-message-or-buffer (before ansi-color activate)
  "Process ANSI color codes in shell output."
  (let ((buf (ad-get-arg 0)))
    (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))))

(defadvice find-tag (before set-tags-directory activate)
  "Ensure the TAGS path is set before searching for tags."
  (setq-local tags-file-name (concat (projectile-project-root) "TAGS")))

(cb:install-package 'emr)
(add-hook 'prog-mode-hook 'emr-initialize)

(bind-key "C-M-<return>" 'emr-show-refactor-menu)

(cb:install-package 'smooth-scrolling t)

(hook-fn 'hs-minor-mode-hook
  (diminish 'hs-minor-mode))

(defadvice hs-minor-mode (around no-errs activate)
  (ignore-errors ad-do-it))

(setq abbrev-file-name (f-join cb:tmp-dir "abbrev_defs"))

(cb:install-package 'dictionary t)

(hook-fn 'auto-revert-mode-hook
  (diminish 'auto-revert-mode))

(winner-mode +1)

(autoload 'transpose-frame "transpose-frame")
(autoload 'flip-frame "transpose-frame")
(autoload 'flop-frame "transpose-frame")
(autoload 'rotate-frame "transpose-frame")
(autoload 'rotate-frame-clockwise "transpose-frame")
(autoload 'rotate-frame-anticlockwise "transpose-frame")

(bind-key "C-x t" 'transpose-frame)
(bind-key "s-t"   'transpose-frame)
(bind-key "C-x f" 'rotate-frame)
(bind-key "s-r"   'rotate-frame)

(defvar-local cb:last-indent-to-column nil)

(defun cb:indent-to-column (arg)
  "Indent point to a certain column.

The first time it is run, prompt for a column. Indent to that
column on subsequent invocations. Prompting can be forced by
suppling a prefix ARG.

Can indent backwards if there is only whitespace."
  (interactive "*P")
  (let ((col (if (or arg (not cb:last-indent-to-column))
                 (read-number "Indent to column: " cb:last-indent-to-column)
               cb:last-indent-to-column)))
    (setq cb:last-indent-to-column col)
    (delete-horizontal-space)
    (indent-to-column col)))

(bind-key "C-c \\"  'cb:indent-to-column)

(cb:declare-package-installer muttrc
  :match "muttrc"
  :packages (muttrc-mode))

(defun byte-compile-conf ()
  "Recompile all configuration files."
  (interactive)
  (dolist (file (file-expand-wildcards (concat user-emacs-directory "*.el")))
    (byte-recompile-file file t))
  (when (boundp 'cb:lisp-dir)
    (byte-recompile-directory cb:lisp-dir 0 t)))

(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)

(defalias 'kb 'kill-buffer)
(defalias 'bb 'bury-buffer)

(defalias 'dfb 'delete-file-and-buffer)
(defalias 'dbf 'delete-file-and-buffer)
(defalias 'rfb 'rename-file-and-buffer)
(defalias 'rbf 'rename-file-and-buffer)

(defalias 'plp 'package-list-packages)

(defalias 'hff 'hexl-find-file)
(defalias 'hex 'hexl-mode)

(defalias 'tsi 'text-scale-increase)
(defalias 'tsd 'text-scale-decrease)

(defalias 'cal 'calendar)

(provide 'config-base)

;;; config-base.el ends here
