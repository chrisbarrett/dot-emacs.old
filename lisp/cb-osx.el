;;; cb-osx

(add-to-list 'default-frame-alist '(font . "Menlo-11"))

(setq Info-default-directory-list
      (--remove (equal "/Applications/Emacs.app" it) Info-default-directory-list)

      system-name (car (split-string system-name "\\.")))

;;; Load shell variables in GUI session.
(when (display-graphic-p)
  (let ((path (shell-command-to-string "source $HOME/.zshrc && printf $PATH")))
    (setenv "PATH" path)
    (setq exec-path (split-string (getenv "PATH") ":"))))

;; Configure cut & paste in terminal.
(unless (display-graphic-p)

  (defun cb:paste ()
    (shell-command-to-string "pbpaste"))

  (defun cb:copy (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function   'cb:copy
        interprogram-paste-function 'cb:paste))

(provide 'cb-osx)
