;;; cb-elisp.el

(defun eval-inplace
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

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

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (font-lock-add-keywords
;;              nil '((cb:match-lets 1 font-lock-keyword-face)
;;                    (cb:match-defs 1 font-lock-keyword-face)
;;                    (cb:match-fns  1 font-lock-function-name-face)))))

(provide 'cb-elisp)
