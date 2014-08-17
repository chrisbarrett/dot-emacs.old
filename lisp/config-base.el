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

;; Set up a good base configuration. This should put Emacs in a state where it
;; is usable so that Emacs can still be used if later configuration files fail.

;;; Code:

(require 'utils-common)
(require 'utils-commands)

;;; Disable unneeded UI.

(when (fboundp 'scroll-bar-mode)   (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)     (tool-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

;; Menu-bar looks acceptable in OS X. Otherwise it adds clutter.
(when (fboundp 'menu-bar-mode)
  (unless (and (eq system-type 'darwin)
               (not noninteractive))
    (menu-bar-mode -1)))

;; Improve GC performance for flx.
(setq gc-cons-threshold (* 1024 1024 20)) ; bytes

;;; Adjust load path.

(add-to-list 'load-path (concat (getenv "HOME") "/Dropbox/"))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "lib"))

;;; Autoloads

(autoload 'fringe-mode "fringe")
(autoload 'toggle-centred-mode "centred-mode")
(autoload 'vc-git-root "vc-git")
(autoload 'transpose-frame "transpose-frame")
(autoload 'flip-frame "transpose-frame")
(autoload 'flop-frame "transpose-frame")
(autoload 'rotate-frame "transpose-frame")
(autoload 'rotate-frame-clockwise "transpose-frame")
(autoload 'rotate-frame-anticlockwise "transpose-frame")

;;; Compatibility

(defalias 'make-local-hook 'ignore)

;;; Set basic variables.

(custom-set-variables
 `(abbrev-file-name                    (f-join cb:tmp-dir "abbrev_defs")            )
 `(auto-save-file-name-transforms      '((".*" ,(concat cb:autosaves-dir "\\1") t)) )
 `(auto-save-list-file-name            (concat cb:autosaves-dir "autosave-list")    )
 `(backup-by-copying                   t                                            )
 `(backup-directory-alist              '(("." . ,cb:autosaves-dir))                 )
 `(bookmark-default-file               (f-join cb:tmp-dir "bookmarks")              )
 `(comint-prompt-read-only             t                                            )
 `(confirm-nonexistent-file-or-buffer  nil                                          )
 `(custom-file                         (concat user-emacs-directory "custom.el")    )
 `(default-input-method                "TeX"                                        )
 `(delete-by-moving-to-trash           nil                                          )
 `(delete-old-versions                 t                                            )
 `(echo-keystrokes                     0.02                                         )
 `(fill-column                         80                                           )
 `(indent-tabs-mode                    nil                                          )
 `(inhibit-startup-message             t                                            )
 `(initial-major-mode                  'org-mode                                    )
 `(initial-scratch-message             nil                                          )
 `(kept-new-versions                   6                                            )
 `(kept-old-versions                   2                                            )
 `(redisplay-dont-pause                t                                            )
 `(require-final-newline               t                                            )
 `(ring-bell-function                  'ignore                                      )
 `(save-place                          t                                            )
 `(save-place-file                     (f-join cb:tmp-dir "saved-places")           )
 `(savehist-additional-variables       '(search ring regexp-search-ring)            )
 `(savehist-autosave-interval          60                                           )
 `(savehist-file                       (f-join cb:tmp-dir "savehist")               )
 `(sentence-end-double-space           nil                                          )
 `(tab-width                           4                                            )
 `(transient-mark-mode                 t                                            )
 `(truncate-partial-width-windows      nil                                          )
 `(version-control                     t                                            )
 `(x-select-enable-clipboard           t                                            )
 `(hippie-expand-try-functions-list    '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
 )

(setq-default major-mode 'org-mode)

;;; Character encodings

(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(defun cb:hide-dos-eol ()
  "Hide DOS EOL chars."
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M [])
  (aset buffer-display-table ?\^L []))

(add-hook 'find-file-hook 'cb:hide-dos-eol)

;;; Configure saveplace.

(require 'saveplace)
(add-hook 'kill-emacs-hook   'save-place-kill-emacs-hook)
(add-hook 'kill-buffer-hook  'save-place-to-alist)
(add-hook 'find-file-hook    'save-place-find-file-hook t)
(add-hook 'server-visit-hook 'save-place-find-file-hook)
(add-hook 'server-done-hook  'save-place-kill-emacs-hook)

;;; Saving behaviour.

(add-hook 'after-save-hook   'executable-make-buffer-file-executable-if-script-p)
(add-hook 'before-save-hook  'whitespace-cleanup)
(add-hook 'before-save-hook  'delete-trailing-whitespace)

;;; Editing advice.

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Do not prompt for confirmation for active processes."
  (noflet ((process-list () nil))
    ad-do-it))

(defadvice whitespace-cleanup (around whitespace-cleanup-indent-tab activate)
  "Use the current buffer's tab settings when cleaning whitespace."
  (let ((whitespace-indent-tabs-mode indent-tabs-mode)
        (whitespace-tab-width tab-width))
    ad-do-it))

(defadvice comment-indent-new-line (after add-space activate)
  "Insert a leading space after comment start for new comment lines."
  (when (and comment-start
             (thing-at-point-looking-at (regexp-quote comment-start)))
    (unless (or (thing-at-point-looking-at (rx (+ space))))
      (just-one-space))))

(defadvice insert-for-yank (after clean-whitespace)
  "Clean up whitespace when inserting yanked text."
  (whitespace-cleanup)
  (delete-trailing-whitespace))

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

(defadvice hs-minor-mode (around no-errs activate)
  "Ignore errors when starting hideshow."
  (ignore-errors ad-do-it))

;;; Diminish modes.

(hook-fn 'auto-revert-mode-hook
  (diminish 'auto-revert-mode))

(hook-fn 'hs-minor-mode-hook
  (diminish 'hs-minor-mode))

;;; Basic interactive commands.

(defvar-local cb:last-indent-to-column nil)

(defun cb:indent-to-column (arg)
  "Indent point to a certain column.

The first time it is run, or when called with prefix ARG, sets
the goal column. Subsequest invocations will indent to that column.

Can indent backwards if there is only whitespace."
  (interactive "*P")
  (cond
   ((or arg (not cb:last-indent-to-column))
    (setq cb:last-indent-to-column (current-column))
    (message "Set goal column to %s" (current-column)))

   (t
    (delete-horizontal-space)
    (indent-to-column cb:last-indent-to-column)
    (message "Indented to column %s" (current-column)))))

(defun byte-compile-conf ()
  "Recompile all configuration files."
  (interactive)
  (dolist (file (file-expand-wildcards (concat user-emacs-directory "*.el")))
    (byte-recompile-file file t))
  (when (boundp 'cb:lisp-dir)
    (byte-recompile-directory cb:lisp-dir 0 t)))

(defun cb:go-away-buffer ()
  "Kill or bury the current buffer"
  (interactive)
  (cond ((< 1 (length (window-list)))
         (kill-current-buffer)
         (delete-window))
        (t
         (kill-current-buffer))))

;;; Install some basic packages

(cb:install-package 'smooth-scrolling t)
(cb:install-package 'dictionary t)

;;; Activate basic modes

(winner-mode +1)
(fringe-mode '(2 . 0))
(auto-compression-mode +1)

(condition-case _
    (savehist-mode +1)
  (void-variable
   (delete-file savehist-file)
   (savehist-mode +1)))

;;; Load EMR

(add-to-list 'load-path "~/Projects/emacs-refactor")
(or (require 'emr nil t) (cb:install-package 'emr))
(add-hook 'prog-mode-hook 'emr-initialize)

;;; Enable commands

(put 'downcase-region  'disabled nil)
(put 'erase-buffer     'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; Global key bindings

(bind-key   "C-M-<return>"   'emr-show-refactor-menu)
(bind-key   "C-c e e"        'toggle-debug-on-error)
(bind-key   "C-x C-\\"       'set-input-method)
(bind-key   "C-x a"          'align-regexp)
(bind-key   "C-x c"          'toggle-centred-mode)
(bind-key   "C-x f"          'rotate-frame)
(bind-key   "C-x t"          'transpose-frame)
(bind-key   "M-C"            'cb:indent-to-column)
(bind-key   "RET"            'comment-indent-new-line)
(bind-key   "s-r"            'rotate-frame)
(bind-key   "s-t"            'transpose-frame)
(bind-key*  "C-<backspace>"  'cb:go-away-buffer)
(bind-key*  "M-/"            'hippie-expand)
(bind-key*  "S-SPC"          'execute-extended-command)

;;; Convenience aliases

(defalias 'bb   'bury-buffer)
(defalias 'dbf  'delete-file-and-buffer)
(defalias 'dfb  'delete-file-and-buffer)
(defalias 'hex  'hexl-mode)
(defalias 'hff  'hexl-find-file)
(defalias 'kb   'kill-buffer)
(defalias 'plp  'package-list-packages)
(defalias 'qr   'query-replace)
(defalias 'qrr  'query-replace-regexp)
(defalias 'rbf  'rename-file-and-buffer)
(defalias 'rfb  'rename-file-and-buffer)
(defalias 'tsd  'text-scale-decrease)
(defalias 'tsi  'text-scale-increase)

(defalias 'cal 'calendar)

(provide 'config-base)

;;; config-base.el ends here
