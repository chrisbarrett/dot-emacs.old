;;; cb-elisp.el

(cb:require-package 'header2)
(cb:require-package 'elisp-slime-nav-mode)
(require 'ert)

(defun eval-inplace
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun cb:byte-compile-elisp ()
  (when (and (equal major-mode 'emacs-lisp-mode)
             (buffer-file-name))
    (byte-compile-file (buffer-file-name))))

;;; Font lock

(defconst cb:match-lets (rx "(" (group (* (not space)) (or "-let" )) word-end))
(defconst cb:match-defs (rx bol (* space) "(" (group
                                               (or "cb:def" "def" "cl-def")
                                               (* (not space)))))
(defconst cb:match-cl   (rx bol (* space) "("
                            (group "cl-def" (or "macro" "un"))
                            (+ space)))
(defconst cb:match-fns  (rx bol (* space) "(" (or "cb:def" "def" "cl-def")
                            (or "n" "un" "macro") (* (not space))
                            (* space)
                            (group (* (not space)))))

(font-lock-add-keywords 'emacs-lisp-mode
                        `((cb:match-lets 1 font-lock-keyword-face)
                          (cb:match-defs 1 font-lock-keyword-face)
                          (cb:match-fns  1 font-lock-function-name-face)))

;;; Hooks

(defun cb:on-emacs-lisp-mode ()
  (local-set-key (kbd "C-c C-t") 'ert)
  (elisp-slime-nav-mode t))

(add-hook 'emacs-lisp-mode-hook 'cb:on-emacs-lisp-mode)
(add-hook 'after-save-hook 'cb:byte-compile-elisp)

(provide 'cb-elisp)
