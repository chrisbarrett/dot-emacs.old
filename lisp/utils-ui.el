;;; utils-ui.el --- UI widgets

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

;; UI widgets

;;; Code:

(require 'utils-common)

(defun deep-replace (target rep tree)
  "Replace TARGET with REP in TREE."
  (cond ((equal target tree) rep)
        ((atom tree)         tree)
        (t
         (--map (deep-replace target rep it) tree))))

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
       (buffer-local-set-key (kbd ,quit-key) (command (kill-buffer) (restore))))))

(cl-defmacro declare-modal-executor
    (name &optional &key command bind restore-bindings)
  "Execute a command with modal window behaviour.

- NAME is used to name the executor.

- COMMAND is a function or sexp to evaluate.

- BIND is a key binding or list thereof used to globally invoke the command.

- RESTORE-BINDINGS are key commands that will restore the buffer
  state. If none are given, BIND will be used as the restore
  key."
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

(defface option-key
  `((t (:foreground "red")))
  "Face for key highlight in search method prompt"
  :group 'options)

(defun cb-lib:columnate-lines (lines column-width)
  "Columnate LINES by splitting the lines into two lists then
zipping them together again, such that:

  '(A B C D)

becomes:

  A C
  B D

COLUMN-WIDTH sets the width of each column."
  (let* ((mid (ceiling (/ (length lines) 2.0)))
         (xs (-slice lines 0 mid))
         (ys (-slice lines mid)))
    (->>
        ;; Add an extra line to YS if there is an odd number of options so
        ;; the zip does not discard an option.
        (if (/= (length xs) (length ys))
            (-concat ys '(""))
          ys)
      (-zip-with
       (lambda (l r) (concat (s-pad-right column-width " " l) r)) xs)
      (s-join "\n"))))

(defun cb-lib:maybe-columnate-lines (thresh-hold column-width lines)
  "Return a formatted string that may columnate the input.
The columnation will occur if LINES exceeds THRESH-HOLD in length.
COLUMN-WIDTH specifies the width of columns if columnation is used."
  (if (< (length lines) thresh-hold)
      (s-join "\n" lines)
    (cb-lib:columnate-lines lines column-width)))

(defun cb-lib:read-opt (option-key-fn options)
  "Read an option from the user.
Returns the element in OPTIONS matching the key event. The \"q\"
key will abort the loop if there is no option bound to \"q\"."
  (let ((c (read-char-choice "" (-concat
                                 (-map (-compose 'string-to-char option-key-fn) options)
                                 (list ?\q)))))
    (or
     ;; Return option with the read key.
     (-first (-compose (~ equal c) 'string-to-char option-key-fn)
             options)
     ;; Cancel if the user had entered \q\ and no option was matched.
     (user-error ""))))

(defun window-bounds ()
  "The width of the selected window, minus the fringe."
  (- (window-width)
     (fringe-columns 'left)
     (fringe-columns 'right)))

(defun read-option (title option-key-fn option-name-fn options)
  "Prompt the user to select from a list of choices.
Return the element in a list of options corresponding to the user's selection.

- TITLE is the name of the buffer that will be displayed.

- OPTION-KEY-FN is a function that returns the key (as a string)
  to use for a given option.

- OPTION-NAME-FN is a function that returns a string describing a given option.

- OPTIONS is a list of items to present to the user."
  (save-excursion
    (save-window-excursion
      ;; Split the window and create a buffer containing the options.

      (let ((win (split-window-below)))
        (select-window win)
        (with-current-buffer (get-buffer-create title)
          (set-window-buffer win (current-buffer))

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

            (erase-buffer)
            (insert
             ;; Show small numbers of options in a single column. If the number
             ;; of lines exceeds 3, split into 2 columns.
             (cb-lib:maybe-columnate-lines 3
                                           (/ (window-bounds) 2)
                                           lines))

            ;; 2. Prepare window.

            (goto-char (point-min))
            (fit-window-to-buffer)

            ;; 3. Read selection from user.
            (unwind-protect
                (cb-lib:read-opt option-key-fn options)
              (kill-buffer title))))))))

(cl-defmacro define-command-picker (name &key title options)
  "Define a command that will display an option picker for the user.

- NAME is the name of the command.

- TITLE is the name of the options buffer to display.

- OPTIONS is a list of options.

Each option is a list of the form (KEY LABEL COMMAND [&key MODES WHEN UNLESS]), where:

- KEY is a string representing the key sequence for the option

- LABEL is a string describing the option

- COMMAND is the command that will be called if this option is selected

- The optional predicates MODES, WHEN and UNLESS control whether
  an option should be displayed. MODES is a symbol or list of
  symbols naming the modes in which the option is available. WHEN
  and UNLESS are nullary functions.

If the predicates are omitted the option will always be shown."
  (cl-assert (not (null options)))
  (cl-assert (stringp title))
  (let ((varname (intern (format "%s-options" name))))
    `(progn

       (defvar ,varname nil ,(format "The list of options shown by `%s'" name))
       (setq ,varname ,options)

       (defun ,name ()
         "Auto-generated option picker."
         (interactive)
         (cl-destructuring-bind (_ _ fn &rest rst)
             (read-option ,title 'car 'cadr
                          ;; Call the predicates for each option to determine
                          ;; whether to display it.
                          (-filter (lambda+
                                     ((&key modes
                                            (when '-true-fn)
                                            (unless '-nil-fn)
                                            &allow-other-keys))
                                     (and
                                      (if modes
                                          (apply 'derived-mode-p (-listify modes))
                                        t)
                                      (funcall when)
                                      (not (funcall unless))))
                                   ,varname))
           ;; Call the option selected by the user.
           (if (commandp fn)
               (call-interactively fn)
             (funcall fn)))))))

(provide 'utils-ui)

;;; utils-ui.el ends here
