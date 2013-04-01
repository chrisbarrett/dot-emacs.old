;;; cb-lisp.el

(cb:require-package 'highlight)
(cb:require-package 'lively)
(cb:require-package 'highlight-parentheses)
(cb:require-package 'highlight-symbol)
(cb:require-package 'volatile-highlights)
(require 'eval-sexp-fu)

;;; Configure lisps

(defun cb:on-lisp-mode ()
  (eval-sexp-fu-flash-mode +1)
  (setq eval-sexp-fu-flash-duration 0.5)
  (volatile-highlights-mode +1)
  (turn-on-eldoc-mode)
  (paredit-mode +1)
  (auto-complete-mode +1)
  (highlight-parentheses-mode +1))

(defun cb:configure-lisp-hook (mode)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook 'cb:on-lisp-mode)
    (add-to-list 'cb:rigid-indent-modes mode)
    (eval-after-load mode
      `(font-lock-add-keywords ',mode '(("\\([()]\\)" . 'paren-face))))))

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
