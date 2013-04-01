;;; cb-compilation.el

(cb:require-package 'mode-compile)

(defun cb:add-compilation-handler (mode handler &optional quitter)
  "Register a set of compilation handlers for the given mode."
  (add-to-list 'mode-compile-modes-alist `(,mode . (,handler ,quitter))))

(defun cb:on-compilation-finish (buf str)
  (unless (string-match "exited abnormally" str)
    (delete-windows-on (get-buffer-create "*compilation"))))

(add-to-list 'compilation-finish-functions 'cb:on-compilation-finish)

(setq mode-compile-expert-p t
      mode-compile-always-save-buffer-p t
      compilation-window-height 12
      compilation-scroll-output 'first-error)

(global-set-key (kbd "C-c C-k") 'mode-compile-kill)
(global-set-key (kbd "C-c C-c") 'mode-compile)

(provide 'cb-compilation)
