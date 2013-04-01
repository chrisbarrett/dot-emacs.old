;;; cb-yasnippet.el

(cb:require-package 'yasnippet)

(defconst cb:yasnippet-dir (concat user-emacs-directory "snippets/"))
(add-to-list 'yas-snippet-dirs cb:yasnippet-dir)
(yas--initialize)
(yas-global-mode t)

(setq yas/trigger-key (kbd "RET"))

(defun cb:on-yasnippet-mode ()
  (setq require-final-newline nil))

(add-hook 'yasnippet-mode-hook 'cb:on-yasnippet-mode)

(provide 'cb-yasnippet)
