;;; cb-flyspell.el

(setq flyspell-issue-message-flag nil)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(provide 'cb-flyspell)
