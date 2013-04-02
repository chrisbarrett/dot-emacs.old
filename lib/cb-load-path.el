;;; cb-load-path.el
;;;
;;; Load path customization functions.

(defun directory-p (f)
  (and (file-directory-p f)
       (not (string-match "/[.]+$" f))))

(defun directory-subfolders (path)
  (->> (directory-files path)
    (--map (concat path it))
    (-filter 'directory-p)))

(defun cb:prepare-load-dir (dir)
  (let ((dir (concat user-emacs-directory dir)))
    (unless (file-exists-p dir) (make-directory dir))
    (--each (cons dir (directory-subfolders dir))
      (add-to-list 'load-path it))
    dir))

(defmacro cb:define-path (sym path)
  "Define a directory that will be added to the lisp `load-path'."
  `(defconst ,sym (cb:prepare-load-dir ,path)))

(provide 'cb-load-path)
