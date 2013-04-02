;;; cb-paredit.el

(defadvice paredit-mode (after disable-autopair activate)
  "Disable autopair while paredit is on."
  (if ad-return-value
      (autopair-mode nil)
    (autopair-on)))

(defun cb:activate-paredit-on-eval-expression ()
  (when (eq this-command 'eval-expression)
    (paredit-mode t)))

(add-hook 'minibuffer-setup-hook 'cb:activate-paredit-on-eval-expression)
(add-hook 'inferior-lisp-mode-hook 'paredit-mode)
(add-hook 'repl-mode-hook 'paredit-mode)

(defun cb:no-space-on-open-round (endp delimiter)
  (not (equal (char-syntax delimiter) ?\()))

(defun cb:paredit-next-top-level-form ()
  (interactive)
  (while (ignore-errors (paredit-backward-up) t))
  (cb:paredit-forward))

(defun cb:paredit-previous-top-level-form ()
  (interactive)
  (if (ignore-errors (paredit-backward-up) t)
      (while (ignore-errors (paredit-backward-up) t))
    (paredit-backward)))

(defun cb:paredit-forward ()
  "Move to the beginning of the next item in the sexp."
  (interactive)
  (if (and (not (paredit-in-string-p))
           (save-excursion
             (ignore-errors
               (forward-sexp)
               (forward-sexp)
               t)))
      (progn
        (forward-sexp)
        (forward-sexp)
        (backward-sexp))
    (paredit-forward)))

(defun cb:paredit-forward-slurp-sexp-neatly ()
  (interactive)
  (save-excursion
    (cond
     ;; Fail if we're in a comment.
     ((or (paredit-in-comment-p)
          (paredit-in-char-p))
      (error "Invalid context for slurping S-expressions."))
     ;; Slurp strings
     ((paredit-in-string-p)
      (paredit-forward-slurp-into-string))
     ;; Else slurp sexp.
     ((save-excursion
        (paredit-forward-up)
        (paredit-backward-down)
        (paredit-forward-slurp-sexp)
        (just-one-space)))))
  ;; Cleanup.
  (when (not (save-excursion
               (ignore-errors
                 (backward-sexp) t)))
    (delete-horizontal-space)))

(provide 'cb-paredit)
