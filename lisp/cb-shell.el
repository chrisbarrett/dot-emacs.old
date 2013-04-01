;;; cb-shell

;;; eshell

(autoload 'eshell/pwd "eshell")

(defun cb:eshell-prompt ()
  (format "%s\n%s"
          (abbreviate-file-name (eshell/pwd))
          (if (= (user-uid) 0) " # " " % ")))

(setq eshell-prompt-function 'cb:eshell-prompt)

;;; shell

(defun cb:fix-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    (set-process-window-size (get-buffer-process (current-buffer))
                             (window-height)
                             (window-width))))

(defun cb:on-shell-mode ()
  ;; add this hook as buffer local, so it runs once per window.
  (add-hook 'window-configuration-change-hook 'cb:fix-window-size nil t))

(add-hook 'shell-mode-hook 'cb:on-shell-mode)

(provide 'cb-shell)
