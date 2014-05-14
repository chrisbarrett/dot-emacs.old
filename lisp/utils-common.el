;;; utils-common.el --- Utilities common to all config files

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Utilities common to all config files

;;; Code:

(defvar cb:use-vim-keybindings? t
  "Set to nil to disable Evil-mode and associated key bindings.")

(defun cb:install-package (pkg &optional require?)
  "Install PKG unless it has already been installed.
Also require the package when REQUIRE? is set."
  (unless (package-installed-p pkg) (package-install pkg))
  (when require? (require pkg nil t)))

(cb:install-package 'dash t)
(cb:install-package 'dash-functional t)
(cb:install-package 's t)
(cb:install-package 'f t)
(cb:install-package 'async t)
(cb:install-package 'noflet t)
(cb:install-package 'diminish t)
(cb:install-package 'bind-key t)

(eval-and-compile
  (defun cblib:quote-if-fn (fn)
    (if (symbolp fn) `',fn fn)))

(defmacro N (fn)
  "Like `-not', but does not require FN to be quoted."
  `(-not ,(cblib:quote-if-fn fn)))

(defmacro ~ (fn &rest args)
  "Like `-partial', but does not require FN to be quoted."
  `(-partial ,(cblib:quote-if-fn fn) ,@args))

(defmacro C (&rest fns)
  "Like `-compose', but does not require FNS to be quoted."
  `(-compose ,@(-map 'cblib:quote-if-fn fns)))

(defun -true-fn (&rest _)
  "Always return t."
  t)

(defun -nil-fn (&rest _)
  "Always return nil."
  nil)

(defun -listify (x)
  "Wrap X in a list if it is not a list."
  (if (listp x)
      x
    (list x)))

(defun -uniq-by (selector-fn list)
  "Remove duplicates in the given sequence using a function.

- SELECTOR-FN takes the current element and returns the item to compare.

- LIST is the sequence to transform."
  ;; Cache the items compared using selector-fn for later comparisons. This
  ;; alleviates the need for an additional traversal.
  (let (transformed)
    (--reduce-r-from
     (let ((cur (funcall selector-fn it)))
       (if (-contains? transformed cur)
           acc
         (push cur transformed)
         (cons it acc)))
     nil
     list)))

(defun -non-null (list)
  "Return the non-nil elements in LIST."
  (-keep 'identity list))

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

(defun s-split-sexps (str)
  "Split STR by sexp boundaries."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    ;; Collect sexps in buffer.
    (let (acc (pt (point-min)))
      (until (eobp)
        (forward-sexp)
        (setq acc (cons (s-trim (buffer-substring pt (point)))
                        acc))
        (setq pt (point)))

      (-remove 's-blank? (nreverse acc)))))

(defmacro until (test &rest body)
  "If TEST yields nil, eval BODY... and repeat.
The order of execution is thus TEST, BODY, TEST, BODY and so on
until TEST returns non-nil."
  (declare (indent 1))
  `(while (not ,test)
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

(defun cb-lib:format-message (category desc body)
  (format "[%s]: %s\n%s\n" category desc body))

(cl-defmacro hook-fn (hook &rest body
                           &key local append (arglist '(&rest _args))
                           &allow-other-keys)
  "Execute forms when a given hook is called.

- HOOK is the name of the hook.

- BODY is a list of forms to evaluate when the hook is run.

- APPEND and LOCAL are passed to the underlying call to `add-hook'.

- ARGLIST overrides the default arglist for the hook's function.

\(fn hook &rest body &key local append arglist)"
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
  "A wrapper for `hook-fn', where HOOKS is a list of hooks.

\(fn hooks &rest body &key local append arglist)"
  (declare (indent 1) (doc-string 2))
  `(progn
     ,@(--map `(hook-fn ',it ,@args)
              (eval hooks))))

(defmacro after (features &rest body)
  "Like `eval-after-load' - once all FEATURES are loaded, execute the BODY.
FEATURES may be a symbol or list of symbols."
  (declare (indent 1))
  ;; Wrap body in a descending list of `eval-after-load' forms.
  ;; The last form is eval'd to remove its quote.
  (eval (->> (-listify (eval features))
          (--map `(eval-after-load ',it))
          (--reduce-from `'(,@it ,acc)
                         `'(progn ,@body)))))

(defmacro command (&rest body)
  "Declare an `interactive' command with BODY forms."
  `(lambda (&optional _arg &rest _args)
     (interactive)
     ,@body))

(defmacro true? (sym)
  "Test whether SYM is bound and non-nil."
  `(and (boundp ',sym) (eval ',sym)))

(cl-defmacro bind-keys (&rest
                        bindings
                        &key map hook overriding?
                        &allow-other-keys)
  "Variadic form of `bind-key'.
- MAP is an optional keymap.  The bindings will only be enabled
  when this keymap is active.

- OVERRIDING? prevents other maps from overriding the binding.  It
  uses `bind-key*' instead of the default `bind-key'.

- HOOK is a hook or list of hooks. The bindings will be made to
  the specified keymap MAP, or using `local-set-key' is no keymap
  is specified.

- BINDINGS are alternating strings and functions to use for
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

- KEYMAP is a keymap to add the bindings to.

- BINDINGS are the bindings to add to the keymap."
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

(cl-defmacro cb:declare-package-installer (group &key match packages)
  "Declare a command to install packages when visiting certain files.

GROUP is a symbol naming the group of packages. It will be used
to create an interactive installation command.

MATCH is a regular expression matched against file names. The
listed packages will be installed automatically when navigating
to a matching file.

PACKAGES is a list of packages that will be installed."
  (declare (indent 1))
  (let ((sym (intern (format "cb:install-%s-packages" group)))
        (n-pkgs (length packages)))
    `(progn
       (defun ,sym ()
         (interactive)

         ;; Prompt the user for confirmation.
         (when (and (called-interactively-p nil)
                    (not
                     (y-or-n-p
                      ,(format "%s package%s will be installed (%s). Continue? "
                               n-pkgs
                               (if (= 1 n-pkgs) "" "s")
                               (s-join ", " (-map 'symbol-name packages))))))
           (user-error "Aborted"))

         (save-window-excursion
           (--each ',packages
             (unless (package-installed-p it)
               (with-demoted-errors "Warning: %s"
                 (package-install it)))
             (ignore-errors
               (require it nil t)))))

       ;; Install automatically on hooks when buffer or file name is a match.
       (hook-fns '(find-file-hook after-change-major-mode-hook)
         (when (s-matches? ,match (buffer-name))
           (funcall ',sym))))))

(defun current-region (&optional no-properties)
  "Return the current active region, or nil if there is no region active.
If NO-PROPERTIES is non-nil, return the region without text properties."
  (when (region-active-p)
    (funcall (if no-properties 'buffer-substring-no-properties 'buffer-substring)
             (region-beginning)
             (region-end))))

(cl-defun current-line (&optional (move-n-lines 0))
  "Return the line at point, or another line relative to this line.
MOVE-N-LINES is an integer that will return a line forward if
positive or backward if negative."
  (save-excursion
    (forward-line move-n-lines)
    (buffer-substring (line-beginning-position) (line-end-position))))

(cl-defun collapse-vertical-whitespace (&optional (to-n-lines 1))
  "Collapse blank lines around point.
TO-N-LINES is the number of blank lines to insert afterwards."
  (interactive "*nCollapse to N blanks: ")
  (save-excursion
    ;; Delete blank lines.
    (search-backward-regexp (rx (not (any space "\n"))) nil t)
    (forward-line 1)
    (while (s-matches? (rx bol (* space) eol) (current-line))
      (forward-line)
      (join-line))
    ;; Open a user-specified number of blanks.
    (open-line to-n-lines)))

(defalias 'qrr 'query-replace-regexp)

(defalias 'kb 'kill-buffer)
(defalias 'bb 'bury-buffer)

(defalias 'dfb 'delete-file-and-buffer)
(defalias 'dbf 'delete-file-and-buffer)
(defalias 'rfb 'rename-file-and-buffer)
(defalias 'rbf 'rename-file-and-buffer)

(defalias 'plp 'package-list-packages)

(defalias 'hff 'hexl-find-file)
(defalias 'hex 'hexl-mode)

(defalias 'tsi 'text-scale-increase)
(defalias 'tsd 'text-scale-decrease)

(defconst user-home-directory    (concat (getenv "HOME") "/"))
(defconst user-dropbox-directory (concat user-home-directory "Dropbox/"))
(defconst user-mail-directory    (f-join user-home-directory "Mail"))

(defmacro define-path (sym path)
  "Define a subfolder of the `user-emacs-directory'.
SYM is declared as a special variable set to PATH.
This directory tree will be added to the load path if ADD-PATH is non-nil."
  `(defconst ,sym
     (let ((dir (f-join user-emacs-directory ,path)))
       (unless (file-exists-p dir) (make-directory dir))
       dir)))

(define-path cb:assets-dir    "assets/")
(define-path cb:autosaves-dir "tmp/autosaves/")
(define-path cb:backups-dir   "backups/")
(define-path cb:bin-dir       "bin/")
(define-path cb:el-get-dir    "el-get")
(define-path cb:elpa-dir      "elpa/")
(define-path cb:etc-dir       "etc/")
(define-path cb:src-dir       "src")
(define-path cb:tmp-dir       "tmp/")
(define-path cb:yasnippet-dir "snippets/")
(define-path cb:info-dir      "info")
(define-path cb:scripts-dir   "scripts/")
(define-path cb:lib-dir       "lib/")
(define-path cb:lisp-dir      "lisp/")

(setq source-directory
      (f-join cb:src-dir (format "emacs-%s.%s"
                                 emacs-major-version
                                 emacs-minor-version)))

(setq org-directory (f-join user-home-directory "org")
      org-default-notes-file (f-join org-directory "notes.org"))

(defvar ledger-file (f-join org-directory "accounts.ledger"))

(-each (->> (list cb:lib-dir cb:lisp-dir)
         (--mapcat (f-directories it nil t)))
  (~ add-to-list 'load-path))

(add-to-list 'exec-path cb:scripts-dir)

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

(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'utils-common)

;;; utils-common.el ends here
