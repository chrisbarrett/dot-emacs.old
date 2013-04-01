;;; cb-fsharp.el

(cb:define-path cb:fsharp-mode-path "lib/fsharp-mode/")
(cb:require-package 'pos-tip)
(or (ignore-errors (require 'fsharp-mode))
    (cb:require-package 'fsharp-mode))

(cb:auto-mode-on-match 'fsharp-mode "\\.fs[iyxl]?*")
(add-to-list 'ac-modes 'fsharp-mode)

(defun cb:on-fsharp-mode ()
  (electric-indent-mode t)
  (electric-layout-mode t)
  (unless (display-graphic-p)
    (setq fsharp-ac-use-popup nil))

  (local-set-key (kbd "M-N") 'next-error)
  (local-set-key (kbd "M-P") 'previous-error))

(add-hook 'fsharp-mode-hook 'cb:on-fsharp-mode)

(provide 'cb-fsharp)
