;;; cb-lib --- Common macros used in my emacs config.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Common macros used in my emacs config.

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 'cl-lib)
(require 'f)
(require 'bind-key)
(require 'noflet)

(defun cb-lib:format-message (category desc body)
  (format "* * * * * *\n[%s]: %s\n%s\n* * * * * *" category desc body))

(defmacro* hook-fn (hook &rest body
                         &key local append (arglist '(&rest _args))
                         &allow-other-keys)
  "Execute forms when a given hook is called.

* HOOK is the name of the hook.

* BODY is a list of forms to evaluate when the hook is run.

* APPEND and LOCAL are passed to the underlying call to
  `add-hook'.

* ARGLIST overrides the default arglist for the hook's function."
  (declare (indent 1) (doc-string 2))

  (cl-assert (symbolp (eval hook)))

  (let ((bod
         ;; Remove keyword args from body.
         `(progn ,@(->> body
                     (-partition-all-in-steps 2 2)
                     (--drop-while (keywordp (car it)))
                     (apply '-concat))))
        (file (or byte-compile-current-file load-file-name)))
    `(progn
       (add-hook ,hook
                 (lambda ,arglist
                   ;; Do not allow errors to propagate from the hook.
                   (condition-case-unless-debug err
                       ,bod
                     (error
                      (message
                       (cb-lib:format-message
                        ,(if file
                             (format "%s in %s" (eval hook) file)
                           hook)
                        "Error raised in hook"
                        (error-message-string err))))))
                 ,append ,local)
       ,hook)))

(defmacro hook-fns (hooks &rest args)
  "A sequence wrapper for `hook-fn'.

* HOOKS is a list of hooks

* ARGS are applied to each call to `hook-fn'."
  (declare (indent 1) (doc-string 2))
  `(progn
     ,@(--map `(hook-fn ',it ,@args)
              (eval hooks))))

(defmacro after (feature &rest body)
  "Like `eval-after-load' - once FEATURE is loaded, execute the BODY."
  (declare (indent 1))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defmacro* configuration-group (&rest
                                body
                                &key when unless after
                                &allow-other-keys)
  "Declare a configuration group for emacs initialisation.

* WHEN and UNLESS make evaluation of the config conditional.

* AFTER is a feature the must be loaded before the config is evaluated.

* The remainder are BODY forms to be executed."
  (let ((bod
         ;; Remove keyword args from body.
         `(progn ,@(->> body
                     (-partition-all-in-steps 2 2)
                     (--drop-while (keywordp (car it)))
                     (apply '-concat))))
        ;; Get the correct name of the file for debugging.
        (file (or byte-compile-current-file load-file-name))
        ;; Make a lisp form that unifies the loading predicates.
        (should-load? (cond ((and when unless)
                             (list 'and when (list 'not unless)))
                            ((not (or when unless))
                             t)
                            (t
                             (list 'or when (list 'not unless))))))

    ;; Arrange body to be evaluated when declared prerequisites are satisfied.
    `(condition-case-unless-debug err
         (when ,should-load?
           ,(if after
                `(after ,after
                   ,bod)
              bod))

       ;; Log error messages.
       (error
        (message
         (cb-lib:format-message
          ,(if file file "dynamic")
          "Error raised in configuration"
          (error-message-string err)))))))

(defmacro command (&rest body)
  "Declare an `interactive' command with BODY forms."
  `(lambda (&optional _arg &rest _args)
     (interactive)
     ,@body))

;;; ----------------------------------------------------------------------------

(defun cb:prepare-load-dir (dir add-path)
  "Create directory DIR if it does not exist.
If ADD-PATH is non-nil, add DIR and its children to the load-path."
  (let ((dir (concat user-emacs-directory dir)))
    (unless (file-exists-p dir) (make-directory dir))
    (when add-path
      (--each (cons dir (f-entries dir 'f-dir? t))
        (add-to-list 'load-path it)))
    dir))

(defmacro define-path (sym path &optional add-path)
  "Define a subfolder of the `user-emacs-directory'.
SYM is declared as a special variable set to PATH.
This directory tree will be added to the load path if ADD-PATH is non-nil."
  `(defconst ,sym (cb:prepare-load-dir ,path ,add-path)))

;;; ----------------------------------------------------------------------------

(defun tree-replace (target rep tree)
  "Replace TARGET with REP in TREE."
  (cond ((equal target tree) rep)
        ((atom tree)         tree)
        (t
         (--map (tree-replace target rep it) tree))))

(defmacro with-window-restore (&rest body)
  "Declare an action that will eventually restore window state.
The original state can be restored by calling (restore) in BODY."
  (declare (indent 0))
  (let ((register (cl-gensym)))
    `(progn
       (window-configuration-to-register ',register)
       ,@(tree-replace '(restore)
                       `(ignore-errors
                          (jump-to-register ',register))
                       body))))

(defmacro* declare-modal-view (command &optional (quit-key "q"))
  "Advise a given command to restore window state when finished."
  `(defadvice ,command (around
                        ,(intern (format "%s-wrapper" command))
                        activate)
     "Auto-generated window restoration wrapper."
     (with-window-restore
       ad-do-it
       (delete-other-windows)
       (local-set-key (kbd ,quit-key) (command (kill-buffer) (restore))))))

(defmacro* declare-modal-executor
    (name &optional &key command bind restore-bindings)
  "Execute a command with modal window behaviour.

* NAME is used to name the executor.

* COMMAND is a function or sexp to evaluate.

* KEY-BINDING is used to globally invoke the command.

* RESTORE-BINDINGS are key commands that will restore the buffer
state.  If none are given, KEY-BINDING will be used as the
restore key."
  (declare (indent defun))
  (let ((fname (intern (format "executor:%s" name))))
    `(progn
       (defun ,fname ()
         ,(format "Auto-generated modal executor for %s" name)
         (interactive)
         (with-window-restore
           ;; Evaluate the command.
           ,(cond ((interactive-form command) `(call-interactively ',command))
                  ((functionp command)        `(funcall #',command))
                  (t                           command))
           (delete-other-windows)
           ;; Configure restore bindings.
           (--each (or ,restore-bindings (list ,bind))
             (buffer-local-set-key (kbd it) (command (bury-buffer) (restore))))))

       (bind-key* ,bind ',fname))))

(defmacro true? (sym)
  "Test whether SYM is bound and non-nil."
  `(and (boundp ',sym) (eval ',sym)))

(defun byte-compile-conf ()
  "Recompile all configuration files."
  (interactive)
  (byte-recompile-file (concat user-emacs-directory "init.el") t 0)
  (when (boundp 'cb:lisp-dir)
    (byte-recompile-directory cb:lisp-dir 0 t)))

;;; ----------------------------------------------------------------------------

(defmacro with-previous-buffer (&rest forms)
  "Execute FORMS within the context of the previous active buffer."
  `(with-current-buffer (nth 1 (buffer-list))
     ,@forms))

(defmacro* --filter-buffers (pred-form &optional (bufs '(buffer-list)))
  "Anaphoric form of `-filter-buffers'"
  `(--filter (with-current-buffer it ,pred-form) ,bufs))

(defmacro* --map-buffers (form &optional (bufs '(buffer-list)))
  "Anaphoric form of `-map-buffers'"
  `(--map (with-current-buffer it ,form) ,bufs))

(defmacro* --first-buffer (pred-form &optional (bufs '(buffer-list)))
  "Anaphoric form of `-first-buffer'"
  `(--first (with-current-buffer it ,pred-form) ,bufs))

(defalias '-first-window 'get-window-with-predicate)

(defmacro --first-window (pred-form)
  "Anaphoric form of `-first-window'.
Find the first window where PRED-FORM is not nil."
  `(-first-window (lambda (it) ,pred-form)))

;; -----------------------------------------------------------------------------

(defmacro* bind-keys (&rest bindings &key map overriding? &allow-other-keys)
  "Variadic form of `bind-key'.
* MAP is an optional keymap.  The bindings will only be enabled when this keymap is active.

* OVERRIDING? prevents other maps from overriding the binding.  It
  uses `bind-key*' instead of the default `bind-key'.

* BINDINGS are alternating strings and functions to use for keybindings."
  (declare (indent 0))
  (let ((bs (->> bindings (-partition-all 2) (--remove (keywordp (car it))))))
    `(progn ,@(loop for (k f) in bs collect
                    (if overriding?
                        `(bind-key* ,k ,f)
                      `(bind-key ,k ,f ,map))))))

(defmacro define-keys (keymap &rest bindings)
  "Variadic form of `define-key'.
* KEYMAP is a keymap to add the bindings to.
* BINDINGS are the bindings to add to the keymap."
  (declare (indent 1))
  (let ((bs (->> bindings (-partition-all 2) (--remove (keywordp (car it))))))
    `(progn
       ,@(loop for (k f) in bs
               collect `(define-key
                          ,keymap
                          ,(if (stringp k) `(kbd ,k) k)
                          ,f)))))

(defun buffer-local-set-key (key command)
  "Map KEY to COMMAND in this buffer alone."
  (interactive "KSet key on this buffer: \naCommand: ")
  (let ((mode-name (intern (format "%s-magic" (buffer-name)))))
    (eval
     `(define-minor-mode ,mode-name
        "Automagically built minor mode to define buffer-local keys."))
    (let* ((mapname (format "%s-map" mode-name))
           (map (intern mapname)))
      (unless (boundp (intern mapname))
        (set map (make-sparse-keymap)))
      (eval
       `(define-key ,map ,key ',command)))
    (funcall mode-name t)))

;; -----------------------------------------------------------------------------

(defun cb:comma-then-space ()
  (interactive)
  (atomic-change-group
    (insert-char ?\,)
    (just-one-space)))

;; -----------------------------------------------------------------------------

(defun s-alnum-only (s)
  "Remove non-alphanumeric characters from S."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (search-forward-regexp (rx (not alnum)) nil t)
      (replace-match ""))
    (buffer-string)))

(defun s-unlines (&rest strs)
  "Join STRS with newlines."
  (s-join "\n" strs))

(defmacro s-lex-cat (&rest format-strs)
  "Concatenate FORMAT-STRS then pass them to `s-lex-format'."
  `(s-lex-format ,(apply 'concat format-strs)))

;; -----------------------------------------------------------------------------

(defun filter-atoms (predicate)
  "Return the elements of the default obarray that match PREDICATE."
  (let (acc)
    (mapatoms (lambda (atom)
                (when (funcall predicate atom)
                  (push atom acc))))
    acc))

(defmacro --filter-atoms (predicate)
  "Anaphoric form of `filter-atoms'.
Return the elements of the default obarray that match PREDICATE."
  `(filter-atoms (lambda (it) ,predicate)))

(defun -listify (x)
  "Wrap X in a list if it is not a list."
  (if (listp x)
      x
    (list x)))

;; -----------------------------------------------------------------------------

(defun make-uuid ()
  "Generate a UUID using the uuid utility."
  (s-trim (shell-command-to-string "uuidgen")))

(defun current-region (&optional no-properties)
  "Return the current active region, or nil if there is no region active.
If NO-PROPERTIES is non-nil, return the region without text properties."
  (when (region-active-p)
    (funcall (if no-properties 'buffer-substring-no-properties 'buffer-substring)
             (region-beginning)
             (region-end))))

(defun current-line (&optional no-properties)
  "Return the line at point.
If NO-PROPERTIES is non-nil, return the line without text properties."
  (funcall (if no-properties 'buffer-substring-no-properties 'buffer-substring)
           (line-beginning-position)
           (line-end-position)))

(defun buffer-length-lines ()
  "Return the number of lines in the current buffer."
  (save-excursion
    (goto-char (point-max))
    (line-number-at-pos)))

(defmacro declare-ido-wrapper (command)
  "Make COMMAND use ido for file and directory completions."
  `(defadvice ,command (around read-with-ido activate)
     (noflet
         ((read-directory-name
           (&rest args) (apply 'ido-read-directory-name args))
          (read-file-name
           (&rest args) (apply 'ido-read-file-name args))
          (read-buffer
           (&rest args) (apply 'ido-read-buffer)))
       ad-do-it)))

(defun* format-progress-bar (pos length &optional (max-len (* 0.75 (window-width))))
  "Draw a progress bar with pips up to POS along its LENGTH.
MAX-LEN constrains the length of the bar.
POS should be a number between 1 and LENGTH."
  (cl-assert (cl-plusp pos))
  (cl-assert (cl-plusp length))
  (cl-assert (cl-plusp max-len))
  (let* ((scaled-length (round (min max-len length)))
         (step-scale (/ scaled-length length))
         (scaled-pos (round (* step-scale pos))))
    (concat "["
            (s-repeat scaled-pos ".")
            (s-repeat (- scaled-length scaled-pos) " ")
            "]")))

(defun run-with-progress-bar (title actions)
  "Call ACTIONS, printing a progress bar with TITLE.
Internal calls to `message' will be suppressed.

String values returned from each action will be shown beside the bar."
  (cl-assert (sequencep actions))
  (cl-loop
   when actions
   with len = (length actions)
   ;; Get the step-number and action.
   for (i . action) in (--map-indexed (cons (1+ it-index) it) actions)
   ;; Run action
   for result = (noflet ((message (&rest _) (ignore))) (funcall action))
   ;; Print progress bar. Do not add it to the *Messages* buffer.
   do (let ((message-log-max nil))
        (message "%s  %s  %s"
                 title
                 (format-progress-bar i len)
                 (or (and (stringp result) result)
                     "")))))

(provide 'cb-lib)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-lib.el ends here
