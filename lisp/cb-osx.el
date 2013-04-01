;;; cb-osx

(add-to-list 'default-frame-alist '(font . "Menlo-11"))

(setq Info-default-directory-list
      (--remove (equal "/Applications/Emacs.app" it) Info-default-directory-list)

      system-name (car (split-string system-name "\\.")))

;;; Load shell variables in GUI session.
(when (display-graphic-p)

  (defun cb:shell->rc-file (path)
    (let ((shell (car (last (split-string path "[/]")))))
      (cond ((equal "zsh" shell)  "~/.zshrc")
            ((equal "bash" shell) "~/.bashrc")
            (t (warn "Unknown shell.")))))

  (defun cb:get-env (file)
    (shell-command-to-string (format ". %s && env" file)))

  (defun cb:parse-env (env)
    (--reduce-from (progn (puthash (first it) (second it) acc) acc)
                   (make-hash-table :test 'equal)
                   (--map (split-string it "[=]") (split-string env))))

  (let* ((vars (cb:parse-env (cb:get-env (cb:shell->rc-file (getenv "SHELL")))))
         (path (split-string (gethash "PATH" vars) "[:]" t)))
    (maphash (lambda (k v) (setenv k v)) vars)
    (setq exec-path path)))

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
