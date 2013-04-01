;;; cb-html

(require 'sgml-mode)
(cb:require-package 'tagedit)

(defun cb:on-html-mode ()
  (tagedit-add-paredit-like-keybindings)
      (setq sgml-xml-mode t))

(add-hook 'html-mode-hook 'cb:on-html-mode)

(provide 'cb-html)
