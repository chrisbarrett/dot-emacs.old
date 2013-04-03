;;; cb-foundation.el
;;;
;;; Basic configuration required for a sane editing environment.

;;; Disable vc modes.
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(eval-after-load "vc"
  '(remove-hook 'find-file-hooks 'vc-find-file-hook))

;;; Use unique buffer names based on file path.
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

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
 color-theme-is-global        t
 ansi-color-for-comint-mode   t
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
  (cl-flet ((process-list ()))
    ad-do-it))

(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "Trim whitespace on kill-line"
  (unless (bolp)
    (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

(defadvice whitespace-cleanup (around whitespace-cleanup-indent-tab activate)
  "Fix whitespace-cleanup indent-tabs-mode bug."
  (let ((whitespace-indent-tabs-mode indent-tabs-mode)
        (whitespace-tab-width tab-width))
    ad-do-it))

;;; Buffers

(defvar cb:kill-buffer-ignored-list '("*scratch*" "*Messages*" "*GROUP*"))

(defun cb:kill-current-buffer ()
  "Kill the current buffer.
If this buffer is a member of `kill-buffer-ignored-list, bury it rather than killing it."
  (interactive)
  (if (member (buffer-name (current-buffer)) cb:kill-buffer-ignored-list)
      (bury-buffer)
    (kill-buffer (current-buffer))))

(global-set-key (kbd "C-x C-K") 'cb:kill-current-buffer)

(defun sudo-edit (&optional arg)
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
