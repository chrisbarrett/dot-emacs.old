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

;; Aliases for combinators

(defalias 'AP 'funcall)
(defalias 'K '-const)
(defalias 'I 'identity)

(eval-and-compile
  (defun cblib:quote-if-fn (fn)
    (if (symbolp fn) `',fn fn)))

(defmacro N (fn)
  "Like `-not', but does not require FN to be quoted."
  `(-not ,(cblib:quote-if-fn fn)))

(defmacro ~ (fn &rest args)
  "Like `-partial', but does not require FN to be quoted."
  `(-partial ,(cblib:quote-if-fn fn) ,@args))

(defmacro ~R (fn &rest args)
  "Like `-rpartial', but does not require FN to be quoted."
  `(-partial ,(cblib:quote-if-fn fn) ,@args))

(defmacro π (&rest fns)
  "Like `-juxt', but does not require FNS to be quoted."
  `(-juxt ,@(-map 'cblib:quote-if-fn fns)))

(defmacro C (&rest fns)
  "Like `-compose', but does not require FNS to be quoted."
  `(-compose ,@(-map 'cblib:quote-if-fn fns)))

(defmacro ^ (fn)
  "Like `-flip', but does not require FN to be quoted."
  `(-flip ,(cblib:quote-if-fn fn)))

(defmacro & (&rest fns)
  "Like `-andfn', but does not require FNS to be quoted."
  `(-andfn ,@(-map 'cblib:quote-if-fn fns)))

(defmacro | (&rest fns)
  "Like `-orfn', but does not require FNS to be quoted."
  `(-orfn ,@(-map 'cblib:quote-if-fn fns)))

(defmacro @ (fn)
  "Like `-applify', but does not require FN to be quoted."
  `(-applify ,(cblib:quote-if-fn fn)))

;; Aliases for compatibility

(defalias 'make-local-hook 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

;;; ----------------------------------------------------------------------------

(defun cb-lib:format-message (category desc body)
  (format "[%s]: %s\n%s\n" category desc body))

(cl-defmacro hook-fn (hook &rest body
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

(defmacro after (features &rest body)
  "Like `eval-after-load' - once all FEATURES are loaded, execute the BODY.
FEATURES may be a symbol or list of symbols."
  (declare (indent 1))
  ;; Wrap body in a descending list of `eval-after-load' forms.
  (->> (-listify (eval features))
    (--map `(eval-after-load ',it))
    (--reduce-from `(,@it ,acc)
                   `'(progn ,@body))))

(cl-defmacro configuration-group (&rest
                                  body
                                  &key when unless after
                                  &allow-other-keys)
  "Declare a configuration group for emacs initialisation.

* WHEN and UNLESS make evaluation of the config conditional.

* AFTER is a feature the must be loaded before the config is evaluated.

* The remainder are BODY forms to be executed."
  (declare (indent 0))
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

    `(condition-case-unless-debug err
         ;; Arrange body to be evaluated when declared prerequisites are
         ;; satisfied.
         (when ,should-load?
           ,(if after
                `(after ,after
                   ,bod)
              bod))
       ;; Log error messages.
       (error
        (message
         (cb-lib:format-message
          ,(or file "dynamic")
          "Error raised in configuration"
          (error-message-string err)))))))

(defmacro command (&rest body)
  "Declare an `interactive' command with BODY forms."
  `(lambda (&optional _arg &rest _args)
     (interactive)
     ,@body))

(defmacro lambda+ (arglist &rest body)
  "A lambda function supporting argument destructuring.

ARGLIST is a full Common Lisp arglist.  Its bindings are availabe
in BODY.

\(fn ARGS [DOCSTRING] [INTERACTIVE] BODY)"
  (declare (doc-string 2) (indent defun)
           (debug (&define lambda-list
                           [&optional stringp]
                           [&optional ("interactive" interactive)]
                           def-body)))
  `(lambda (&rest args)
     (cl-destructuring-bind ,arglist args
       ,@body)))

(defmacro until (test &rest body)
  "If TEST yields nil, eval BODY... and repeat.
The order of execution is thus TEST, BODY, TEST, BODY and so on
until TEST returns non-nil."
  (declare (indent 1))
  `(while (not ,test)
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

(defun deep-replace (target rep tree)
  "Replace TARGET with REP in TREE."
  (cond ((equal target tree) rep)
        ((atom tree)         tree)
        (t
         (--map (deep-replace target rep it) tree))))

(defun assoc-in (path alist)
  "Traverse ALIST along the given PATH of keys using `assoc'.
Return nil if the path cannot be followed."
  (if (and path alist)
      (cl-destructuring-bind (cur &rest next) path
        (assoc-in next (cdr (assoc cur alist))))
    alist))

(defmacro with-window-restore (&rest body)
  "Declare an action that will eventually restore window state.
The original state can be restored by calling (restore) in BODY."
  (declare (indent 0))
  (let ((register (cl-gensym)))
    `(progn
       (window-configuration-to-register ',register)
       ,@(deep-replace '(restore)
                       `(ignore-errors
                          (jump-to-register ',register))
                       body))))

(cl-defmacro declare-modal-view (command &optional (quit-key "q"))
  "Advise a given command to restore window state when finished."
  `(defadvice ,command (around
                        ,(intern (format "%s-wrapper" command))
                        activate)
     "Auto-generated window restoration wrapper."
     (with-window-restore
       ad-do-it
       (delete-other-windows)
       (local-set-key (kbd ,quit-key) (command (kill-buffer) (restore))))))

(cl-defmacro declare-modal-executor
    (name &optional &key command bind restore-bindings)
  "Execute a command with modal window behaviour.

* NAME is used to name the executor.

* COMMAND is a function or sexp to evaluate.

* BIND is a key binding or list thereof used to globally invoke the command.

* RESTORE-BINDINGS are key commands that will restore the buffer
state.  If none are given, BIND will be used as the
restore key."
  (declare (indent defun))
  (let ((fname (intern (format "executor:%s" name)))
        (bindings (if (listp bind) bind `'(,bind))))
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
           (--each (or ,restore-bindings ,bindings)
             (buffer-local-set-key (kbd it) (command (bury-buffer) (restore))))))

       ;; Create global hotkeys
       (--each ,bindings
         (eval `(bind-key* ,it ',',fname))))))

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

(cl-defmacro --filter-buffers (pred-form &optional (bufs '(buffer-list)))
  "Anaphoric form of `-filter-buffers'"
  `(--filter (with-current-buffer it ,pred-form) ,bufs))

(cl-defmacro --map-buffers (form &optional (bufs '(buffer-list)))
  "Anaphoric form of `-map-buffers'"
  `(--map (with-current-buffer it ,form) ,bufs))

(cl-defmacro --first-buffer (pred-form &optional (bufs '(buffer-list)))
  "Anaphoric form of `-first-buffer'"
  `(--first (with-current-buffer it ,pred-form) ,bufs))

(defalias '-first-window 'get-window-with-predicate)

(defmacro --first-window (pred-form)
  "Anaphoric form of `-first-window'.
Find the first window where PRED-FORM is not nil."
  `(-first-window (lambda (it) ,pred-form)))

;; -----------------------------------------------------------------------------

(cl-defmacro bind-keys (&rest
                        bindings
                        &key map hook overriding?
                        &allow-other-keys)
  "Variadic form of `bind-key'.
* MAP is an optional keymap.  The bindings will only be enabled
  when this keymap is active.

* OVERRIDING? prevents other maps from overriding the binding.  It
  uses `bind-key*' instead of the default `bind-key'.

* HOOK is a hook or list of hooks. The bindings will be made to
  the specified keymap MAP, or using `local-set-key' is no keymap
  is specified.

* BINDINGS are alternating strings and functions to use for
  keybindings."
  (declare (indent 0))
  (cl-assert (not (and map overriding?)))
  (let ((bs (->> bindings (-partition-all 2) (--remove (keywordp (car it))))))
    `(progn
       ,@(cl-loop for (k f) in bs collect
                  (cond
                   (overriding?
                    `(bind-key* ,k ,f))
                   (hook
                    `(hook-fns ',(-listify hook)
                       ;; If there is a map specified, bind to that
                       ;; map. Otherwise fall back on `local-set-key' for
                       ;; bindings.
                       (if (true? ,map)
                           (bind-key ,k ,f ,map)
                         (local-set-key ,k ,f))))
                   (t
                    `(bind-key ,k ,f ,map)))))))

(defmacro define-keys (keymap &rest bindings)
  "Variadic form of `define-key'.
* KEYMAP is a keymap to add the bindings to.
* BINDINGS are the bindings to add to the keymap."
  (declare (indent 1))
  (let ((bs (->> bindings (-partition-all 2) (--remove (keywordp (car it))))))
    `(progn
       ,@(cl-loop for (k f) in bs
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

(defmacro s-with-temp-buffer (&rest body)
  "Evaluate BODY in a temporary buffer and return the buffer string."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     ,@body
     (buffer-string)))

(defalias 's-no-props 'substring-no-properties)

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

(defmacro with-open-file (file &rest body)
  "Visit FILE and return the result of evaluating BODY forms.
Delete the buffer if it was not already visited."
  (declare (indent 1))
  (let ((gbuf (cl-gensym)))
    `(save-excursion
       (let ((,gbuf (get-buffer ,file)))
         (cond
          ;; Visit existing buffer if it's active.
          ((and ,gbuf (buffer-live-p ,gbuf))
           (with-current-buffer ,gbuf
             ,@body))
          ;; Otherwise, visit the file in a new buffer.
          (t
           ;; Rebind `kill-buffer-query-functions' to ensure no interactive
           ;; prompts are issued.
           (let ((kill-buffer-query-functions nil))
             (find-file ,file)
             (unwind-protect (save-excursion ,@body)
               ;; Unconditionally kill the buffer.
               (kill-buffer)))))))))

(cl-defun buffer-string-no-properties (&optional (buffer (current-buffer)))
  "Return the contents of BUFFER without text properties.
If BUFFER is nil, the current buffer is used."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

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

(defun cb:append-buffer ()
  "Enter insertion mode at the end of the current buffer."
  (interactive)
  (goto-char (point-max))
  (if (fboundp 'evil-append-line)
      (evil-append-line 1)
    (end-of-line)))

;;; ----------------------------------------------------------------------------
;;; UI

(cl-defun format-progress-bar (title pos length)
  "Format a progress bar with TITLE and pips up to POS along its LENGTH.
POS should be a number between 1 and LENGTH."
  (cl-assert (cl-plusp pos))
  (cl-assert (cl-plusp length))
  (let* (
         ;; Format title and percentage.
         (leader (concat title " "))
         (percentage (round (* 100 (/ (float pos) length))))
         (trailer (concat " " (number-to-string percentage) "%"))
         ;; Compute the max length of the bar, including the leader and title.
         (max-len (- (1- (window-width))
                     (+ (length leader) (length trailer))))
         ;; Constrain the progress meter to fit within MAX-LEN columns.
         (scaled-length (round (min max-len length)))
         (step-scale (/ scaled-length length))
         (scaled-pos (round (* step-scale pos))))
    (concat leader
            (s-repeat scaled-pos ".")
            (s-repeat (- scaled-length scaled-pos) " ")
            trailer)))

(cl-defun run-with-progress-bar
    (title actions &key (silent? t) &allow-other-keys)
  "Call ACTIONS, printing a progress bar with TITLE.

By default, calls to `message' in each action will be suppressed.
Use the SILENT? keyword to explicitly override this behaviour.

In batch mode, this just prints a summary instead of progress."
  (cl-assert (stringp title))
  (cl-assert (sequencep actions))
  (cl-loop
   when actions

   initially (when noninteractive (message "%s..." title))
   finally   (when noninteractive (message "%s...done" title))

   ;; Get the step-number and action.
   with len = (length actions)
   for (i . action) in (--map-indexed (cons (1+ it-index) it) actions)
   ;; Run action.
   do (if silent?
          (noflet ((message (&rest _) (ignore))) (funcall action))
        (funcall action))
   ;; Print progress bar. Do not add it to the *Messages* buffer.
   do (unless noninteractive
        (let ((message-log-max nil))
          (message "%s" (format-progress-bar title i len))))))

(defface option-key
  `((t (:foreground "red")))
  "Face for key highlight in search method prompt"
  :group 'options)

(defun cb-lib:maybe-columnate-lines (thresh-hold column-width lines)
  "Return a formatted string that may columnate the input.
The columnation will occur if LINES exceeds THRESH-HOLD in length.
COLUMN-WIDTH specifies the width of columns if columnation is used."
  (if (< (length lines) thresh-hold)
      (s-join "\n" lines)
    ;; Columnate lines by splitting the lines into two lists then zipping them
    ;; together again, such that:
    ;;
    ;; '("A" "B" "C" "D")
    ;;
    ;; becomes the string:
    ;;
    ;; A  C
    ;; B  D
    ;;
    (let* ((mid (floor (/ (length lines) 2.0)))
           (xs (-slice lines 0 mid))
           (ys (-slice lines mid (length lines))))
      (->>
          ;; Add an extra line to YS if there is an odd number of options so
          ;; the zip does not discard an option.
          (if (/= (length xs) (length ys))
              (-concat ys '(""))
            ys)
        (-zip-with
         (lambda (l r) (concat (s-pad-right column-width " " l) r)) xs)
        (s-join "\n")))))

(defun read-option (title option-key-fn option-name-fn options)
  "Prompt the user to select from a list of choices.
Return the element in OPTIONS corresponding to the user's selection.

* TITLE is the name of the buffer that will be displayed.

* OPTIONS is a list of items to present to the user.

* OPTION-KEY-FN is a function that returns the key (as a string)
  to use for a given option.

* OPTION-NAME-FN is a function that returns a string describing a given option."
  (unwind-protect
      (save-excursion
        (save-window-excursion
          ;; Create a buffer containing methods.
          (switch-to-buffer-other-window (get-buffer-create title))
          (erase-buffer)

          ;; 1. Format the options for insertion.

          (let* ((longest-key
                  (-max (-map (-compose 'length option-key-fn) options)))
                 ;; Transform the options list into a list of lines of
                 ;; "[key] desc"
                 (lines
                  (->> options
                    (-sort (-on 'string< (-compose 's-downcase option-key-fn)))
                    (--map
                     (let ((key
                            (propertize (funcall option-key-fn it) 'face 'option-key)))
                       (format " %s %s"
                               (s-pad-right
                                (+ 2 longest-key) ; Offset by length of square brackets.
                                " " (concat "[" key "]"))
                               (funcall option-name-fn it)))))))

            (insert
             ;; Show small numbers of options in a single column. If the number
             ;; of lines exceeds 3, split into 2 columns.
             (cb-lib:maybe-columnate-lines
              3
              (/ (- (window-width)
                    (fringe-columns 'left)
                    (fringe-columns 'left)) 2)
              lines))

            ;; 2. Prepare window.

            (goto-char (point-min))
            (fit-window-to-buffer)

            ;; 3. Read selection from user.

            (cl-loop
             while t

             for key =
             (let ((inhibit-quit t))
               (read-char-exclusive))

             for method =
             (-first (-compose (~ equal key) 'string-to-char option-key-fn)
                     options)
             do
             (cond
              (method
               (return method))
              ((-contains? '(?\C-g ?q) key)
               (setq quit-flag t))
              (t
               (message "Invalid key")))))))

    (kill-buffer title)))

;;; ----------------------------------------------------------------------------
;;; Growl Notifications

(defvar growl-program "growlnotify")

(cl-defun growl (title
                 message
                 &optional (icon "/Applications/Emacs.app/Contents/Resources/Emacs.icns"))
  "Display a message and a growl notification on localhost.
The notification will have the given TITLE and MESSAGE."
  (message "%s. %s" title message)
  (when (executable-find growl-program)
    (start-process "growl" " growl"
                   growl-program
                   title
                   "-n" "Emacs"
                   "-a" "Emacs"
                   "--image" icon)
    (process-send-string " growl" message)
    (process-send-string " growl" "\n")
    (process-send-eof " growl")))

;;; ----------------------------------------------------------------------------

(defvar %-sudo-liftable-commands '(%-sh
                                   %-async
                                   %-string
                                   shell-command
                                   async-shell-command
                                   shell-command-to-string)
  "A list of commands that may be escalated using the `%-sudo' macro.

`%-sudo' operates by modifying the string passed to the shell.
For this to work, all commands in this list must accept a string
as their first parameter.")

(defalias '%-quote 'shell-quote-argument)

(defun %-sh (command &rest arguments)
  "Run COMMAND with ARGUMENTS, returning the exit code."
  (shell-command (concat command " " (s-join " " arguments))))

(defun %-string (command &rest arguments)
  "Run COMMAND with ARGUMENTS, returning its output as a string."
  (s-trim-right
   (shell-command-to-string (concat command " " (s-join " " arguments)))))

(defun %-async (command &rest arguments)
  "Run COMMAND with ARGUMENTS asynchronously."
  (async-shell-command (concat command " " (s-join " " arguments))))

(defun %-can-sudo-without-passwd? ()
  "Test whether we are currently able to sudo without entering a password."
  (zerop (shell-command "sudo -n true")))

(defmacro %-sudo (command)
  "Execute a shell command with escalated privileges.

COMMAND must be a direct call to one of the forms listed in
`sudo-liftable-commands'.

The sudo command will likely be configured with a timeout on your
system.  The user will be interactively prompted for their
password if necessary.  Subsequent calls to sudo within the
timeout period will not require the password again."
  (cl-assert command)
  (cl-assert (listp command))
  (cl-assert (-contains? %-sudo-liftable-commands (car command)))

  ;; Reach into the command and replace the direct shell command argument,
  ;; wrapping it with a call to sudo.
  ;;
  ;; There are two execution paths, depending on whether the user is currently
  ;; authenticated with sudo.
  (cl-destructuring-bind (fn cmd &rest args) command
    (let ((g-passwd (cl-gensym))
          (g-result (cl-gensym)))
      `(-if-let (,g-passwd (unless (%-can-sudo-without-passwd?)
                             (read-passwd "Password: ")))

           ;; Path 1. The password is required: Consume the password and
           ;; tidy the shell output. Finally, delete the password string from
           ;; memory.
           (unwind-protect
               (let ((,g-result
                      (,fn
                       (format "echo %s | sudo -S %s"
                               (shell-quote-argument ,g-passwd) ,cmd)
                       ,@args)))
                 ;; Annoyingly, the password prompt gets prepended to string
                 ;; output and must be stripped.
                 (if (stringp ,g-result)
                     (s-chop-prefix "Password:" ,g-result)
                   ,g-result))
             ;; Clear the password from memory.
             (clear-string ,g-passwd))

         ;; Path 2. We are within the sudo timeout period: The password is not
         ;; required and we can call the command with sudo prefixed.
         (,fn (format "sudo %s" ,cmd) ,@args)))))

;;; ----------------------------------------------------------------------------

(defun generate-password (length)
  "Generate a password with a given LENGTH."
  (interactive (list (read-number "Password length: " 32)))
  (let ((pass
         (--> (%-string "openssl" "rand" "-base64" (number-to-string length))
           ;; The encoding process will pad with '=' characters to reach a
           ;; length divisible by 4 bytes. Drop this padding.
           (substring it 0 length))))
    (cond
     ((called-interactively-p 'any)
      (kill-new pass)
      (message "Password copied to kill ring."))
     (t
      pass))))

(defun make-uuid ()
  "Generate a UUID using the uuid utility."
  (s-trim (shell-command-to-string "uuidgen")))

;;; ----------------------------------------------------------------------------

(defun euro-date->iso-date (str)
  "Convert a date STR of form dd/mm/yy to yy-mm-dd."
  (cl-destructuring-bind (day month year)
      (s-split "/" str)
    (format "%s-%s-%s" year month day)))

(provide 'cb-lib)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-lib.el ends here
