;;; cb-macros.el
;;;
;;; Common macros for emacs configuration.

(defun macro-boundp (symbol)
  "Test whether SYMBOL is bound as a macro."
  (and (fboundp symbol)
       (eq (car (symbol-function symbol)) 'macro)))

(defmacro defmacro-safe (symbol arglist &rest body)
  "Define the given macro only if it is not already defined."
  (declare (doc-string 3) (indent defun))
  (cl-assert (symbolp symbol))
  (cl-assert (listp arglist))
  `(unless (macro-boundp ',symbol)
     (cl-defmacro ,symbol ,arglist ,@body)))

(defmacro-safe when-let ((var form) &rest body)
  "Execute BODY forms with bindings only if FORM evaluates to a non-nil value."
  (declare (indent 1))
  `(let ((,var ,form))
     (when ,var
       ,@body)))

(defmacro-safe if-let ((var form) then &rest else)
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


(provide 'cb-macros)
