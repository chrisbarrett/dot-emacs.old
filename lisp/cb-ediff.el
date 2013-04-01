;;; cb-ediff

(setq diff-switches "-u"
      ediff-window-setup-function 'ediff-setup-windows-plain)

(add-hook 'ediff-startup-hook 'turn-off-evil-mode)

;; Emacs as mergetool.
;; See http://stackoverflow.com/questions/1817370/using-ediff-as-git-mergetool

(defun cb:apply-diff ()
  (let ((file ediff-merge-store-file))
    (set-buffer ediff-buffer-C)
    (write-region (point-min) (point-max) file)
    (message "Merge buffer saved in: %s" file)
    (set-buffer-modified-p nil)
    (sit-for 1)))

(defun cb:handle-git-merge (local remote base merged)
  "Configure this emacs session for use as the git mergetool."
  (add-hook 'ediff-quit-hook 'kill-emacs)
  (add-hook 'ediff-quit-merge-hook 'cb:apply-diff)
  (ediff-merge-files-with-ancestor local remote base nil merged))

(provide 'cb-ediff)
