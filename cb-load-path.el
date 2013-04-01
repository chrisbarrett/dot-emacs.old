;;; cb-load-path.el
;;;
;;; Load path customization.

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
      (message "--> add to load path: %s" it)
      (add-to-list 'load-path it))
    dir))

(cl-defmacro cb:define-path (sym path)
  "Define a directory that will be added to the lisp `load-path'."
  `(defconst ,sym (cb:prepare-load-dir ,path)))

(cb:define-path user-lib-dir "lib/")
(cb:define-path user-lisp-dir "lisp/")
(cb:define-path user-tmp-dir "tmp/")
(cb:define-path user-etc-dir "etc/")
(cb:define-path user-bin-dir "bin/")

(provide 'cb-load-path)
