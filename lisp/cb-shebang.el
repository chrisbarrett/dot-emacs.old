;;; cb-shebang.el

(defconst cb:extension->cmd (make-hash-table :test 'equal))
(puthash "py" "python" cb:extension->cmd)
(puthash "sh" "bash"   cb:extension->cmd)
(puthash "rb" "ruby"   cb:extension->cmd)
(puthash "el" "emacs"  cb:extension->cmd)

(defun cb:bufname->cmd (name)
  (gethash (car-safe (last (split-string name "[.]" t)))
           cb:extension->cmd))

(defun insert-shebang ()
  "Insert a shebang line at the top of the current buffer."
  (interactive)
  (let* ((env (shell-command-to-string "where env"))
         (env (replace-regexp-in-string "[\r\n]*" "" env))
         (cmd (cb:bufname->cmd buffer-file-name)))
    (save-excursion
      (goto-char (point-min))
      (insert (concat "#!" env " " cmd))
      (newline 2))))

(provide 'cb-shebang)
