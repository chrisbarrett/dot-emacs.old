;;; cb-lisp.el

(defun cb:configure-lisp-hook (mode)
  (hook-fn (intern (concat (symbol-name mode) "-hook"))
    (eval-sexp-fu-flash-mode +1)
    (setq eval-sexp-fu-flash-duration 0.5)
    (volatile-highlights-mode +1)
    (turn-on-eldoc-mode)
    (paredit-mode +1)
    (auto-complete-mode +1)
    (highlight-parentheses-mode +1))
  (eval-after-load mode
    `(font-lock-add-keywords ',mode '(("\\([()]\\)" . 'paren-face)))))

(-each '(scheme-mode
         emacs-lisp-mode
         lisp-mode
         common-lisp-mode
         repl-mode
         clojure-mode
         clojurescript-mode
         ielm-mode)
       'cb:configure-lisp-hook)

(provide 'cb-lisp)
