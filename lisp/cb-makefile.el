;;; cb-makefile.el

(add-to-list 'ac-modes 'makefile-mode)

(defun cb:on-makefile-mode ()
  (auto-complete-mode t)
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'cb:on-makefile-mode)

(defadvice whitespace-cleanup (around whitespace-cleanup-indent-tab activate)
  "Fix whitespace-cleanup indent-tabs-mode bug in emacs 24.2"
  (let ((whitespace-indent-tabs-mode indent-tabs-mode)
        (whitespace-tab-width tab-width))
    ad-do-it))

(provide 'cb-makefile)
