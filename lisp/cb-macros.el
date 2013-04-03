;;; cb-macros.el
;;;
;;; Common macros for emacs configuration.

(defun macro-boundp (symbol)
  "Test whether SYMBOL is bound as a macro."
  (and (fboundp symbol)
       (eq (car (symbol-function symbol)) 'macro)))

(defmacro cb:defmacro-safe (symbol arglist &rest body)
  "Define the given macro only if it is not already defined."
  (declare (doc-string 3) (indent defun))
  (cl-assert (symbolp symbol))
  (cl-assert (listp arglist))
  `(unless (macro-boundp ',symbol)
     (cl-defmacro ,symbol ,arglist ,@body)))

(cb:defmacro-safe when-let ((var form) &rest body)
  "Execute BODY forms with bindings only if FORM evaluates to a non-nil value."
  (declare (indent 1))
  `(let ((,var ,form))
     (when ,var
       ,@body)))

(cb:defmacro-safe if-let ((var form) then &rest else)
  "Execute THEN form with bindings if FORM evaluates to a non-nil value,
otherwise execute ELSE forms without bindings."
  (declare (indent 1))
  `(let ((,var ,form))
     (if ,var ,then ,@else)))

(defmacro hook-fn (hook &optional docstring &rest body)
  "Execute BODY forms when HOOK is called. The arguments passed
to the hook function are bound to the symbol 'args'."
  (declare (indent 1) (doc-string 2))
  `(add-hook ,hook (lambda (&rest args)
                     ,@(cons docstring body))))

;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------

(defun cb:ensure-package (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(defmacro cb:use-package (name &rest args)
  (cl-assert (symbolp name))
  (let ((args* (cl-gensym)))
    `(eval-and-compile
       (let ((,args* ,args))
         (when-let (pkg (plist-get ,args* ))
           (cb:ensure-package ,name))
         (use-package ,name ,@args)))))

(provide 'cb-macros)
