;;; cb-elisp.el

(defun eval-and-replace ()
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;; Font lock

(font-lock-add-keywords
 'emacs-lisp-mode
 `(
   ;; -let forms.

   (,(rx "(" (group (* (not space)) "-let") symbol-end)
    (1 font-lock-keyword-face))

   ;; cl-definition forms.

   (,(rx "(" (group (or "cl-defun" "cl-defmacro" "cb:def")
                    (* (not space)))
         (+ space)
         (group (+ (regex "\[^ )\n\]"))))
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))

   ;; cl-struct.

   (,(rx "(" (group "cl-defstruct")
         (+ space)
         (group (+ (regex "\[^ )\n\]"))))
    (1 font-lock-keyword-face)
    (2 font-lock-type-face))

   ;; use-package macro.

   (,(rx "(" (group "use-package")
         (+ space)
         (group (+ (regex "\[^ )\n\]"))))
    (1 font-lock-keyword-face)
    (2 font-lock-constant-face))))

(provide 'cb-elisp)
