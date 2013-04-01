;;; cb-lambda.el

(require 'lambda-mode)

(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))
(add-hook 'inferior-lisp-mode-hook 'lambda-mode)
(add-hook 'lisp-mode-hook 'lambda-mode)
(add-hook 'emacs-lisp-mode-hook 'lambda-mode)
(add-hook 'python-mode-hook 'lambda-mode)
(add-hook 'slime-repl-mode-hook 'lambda-mode)

(provide 'cb-lambda)
