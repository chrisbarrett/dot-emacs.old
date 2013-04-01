;;; cb-flycheck.el

(cb:require-package 'flycheck)

(defun cb:maybe-enable-flycheck ()
  (when (flycheck-may-enable-mode)
    (unless (equal major-mode 'emacs-lisp-mode)
      (flycheck-mode t))))

(defun cb:on-flycheck-mode ()
  (setq flycheck-highlighting-mode 'lines)
  (local-set-key (kbd "M-N") 'next-error)
  (local-set-key (kbd "M-P") 'previous-error))

(add-hook 'prog-mode-hook 'cb:maybe-enable-flycheck)
(add-hook 'text-mode-hook 'cb:maybe-enable-flycheck)
(add-hook 'flycheck-mode-hook 'cb:on-flycheck-mode)

(provide 'cb-flycheck)
