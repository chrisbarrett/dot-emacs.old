;;; cb-load-path.el
;;;
;;; Load path customization.

(defun cb:prepare-load-dir (dir absolute? create?)
  (let ((dir (expand-file-name
              (if absolute? dir (concat user-emacs-directory dir)))))
    (when (and create? (not (file-exists-p dir)))
      (make-directory dir))
    (add-to-list 'load-path dir)
    dir))

(cl-defmacro cb:define-path
    (sym path &key absolute? create? &allow-other-keys)
  "Define a directory that will be added to the lisp `load-path'."
  `(defconst ,sym ,(cb:prepare-load-dir path absolute? create?)))

(cb:define-path user-lib-dir  "lib/"  :create t)
(cb:define-path user-lisp-dir "lisp/" :create t)
(cb:define-path user-tmp-dir  "tmp/"  :create t)

(provide 'cb-load-path)
