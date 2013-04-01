;;; cb-text

(defun cb:on-text-mode ()
  (auto-complete-mode nil))

(add-hook 'text-mode-hook 'cb:on-text-mode)

(provide 'cb-text)
