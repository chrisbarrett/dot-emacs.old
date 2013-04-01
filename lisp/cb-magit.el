;;; cb-magit

(cb:require-package 'magit)

(cb:auto-mode-on-match 'conf-mode ".gitignore")
(global-set-key (kbd "C-c g") 'magit-status)
(define-key magit-status-mode-map (kbd "q") 'cb:quit-magit)

(defun cb:enter-insertion-mode ()
  (when (and (featurep 'evil) evil-mode)
    (evil-append-line nil)))

(add-hook 'magit-mode-hook 'magit-load-config-extensions)
(add-hook 'magit-log-edit-mode-hook 'cb:enter-insertion-mode)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun cb:quit-magit ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(provide 'cb-magit)
