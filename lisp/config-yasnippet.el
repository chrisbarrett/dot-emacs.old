;;; config-yasnippet.el --- Configuration for yasnippet  -*- lexical-binding: t; -*-

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

;; Configuration for yasnippet

;;; Code:

(require 'utils-common)
(require 'utils-ui)
(require 'config-theme)
(require 'utils-commands)

(cb:install-package 'yasnippet t)
(require 'yasnippet)

(custom-set-variables
 '(yas-snippet-dirs (list cb:yasnippet-dir))
 '(yas-prompt-functions '(yas-ido-prompt))
 '(yas-wrap-around-region t)
 '(yas-verbosity 0))

(custom-set-faces
 '(yas-field-highlight-face
   ((((background light)) :background "lightgreen")
    (((background dark)) :background "green4" :foreground "grey80"))))

(diminish 'yas-minor-mode)

(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'text-mode-hook 'yas-minor-mode)

(noflet ((message (&rest _))) (yas-global-mode))

(hook-fn 'snippet-mode-hook
  (setq-local require-final-newline nil))

;;; FIX: yasnippet often errors when trying to save existing snippets.

(defun cbyas:other-buffer-major-mode ()
  "Guess the mode to use for a snippet.
Use the mode of the last editing buffer."
  (with-current-buffer (-first (-not 'minibufferp) (cdr (buffer-list)))
    major-mode))

(defun cbyas:new-snippet? (template)
  "Return whether TEMPLATE should be saved as a new snippet.

Only offer to save this if it looks like a library or new
snippet (loaded from elisp, from a dir in `yas-snippet-dirs'which
is not the first, or from an unwritable file)."
  (or (not (yas--template-file template))
      (not (f-writable? (yas--template-file template)))
      (and (listp yas-snippet-dirs)
           (< 1 (length yas-snippet-dirs))
           (not (f-child-of? (yas--template-file template)
                             (car yas-snippet-dirs))))))

(defun cbyas:create-dir-for-template (template)
  (-when-let* ((snippet-dirs (yas--guess-snippet-directories (yas--template-table template))))
    (yas--make-directory-maybe (car snippet-dirs))))

(defun cbyas:snippet-file-name (template)
  (-if-let (file (yas--template-file template))
      (f-filename file)
    (yas--template-name template)))

(defun cbyas:maybe-write-new-template (template)
  (cl-assert template () "Attempting to access null yas template")
  (when (cbyas:new-snippet? template)
    (-when-let* ((snippet-dir (cbyas:create-dir-for-template template))
                 (file-name (cbyas:snippet-file-name template)))
      (write-file (f-join snippet-dir file-name))
      (setf (yas--template-file template) (buffer-file-name)))))

(after 'yasnippet
  (defun yas--read-table ()
    "Ask user for a snippet table, help with some guessing."
    (let ((modes (-distinct (-snoc (yas--compute-major-mode-and-parents (buffer-file-name))
                                   (cbyas:other-buffer-major-mode)))))
      (intern (completing-read "Choose or enter a mode: " modes))))

  (defun yas-load-snippet-buffer-and-close (table &optional _)
    "Load the snippet with `yas-load-snippet-buffer', possibly
  save, then `quit-window' if saved.

If the snippet is new, ask the user whether (and where) to save
it. If the snippet already has a file, just save it.

Don't use this from a Lisp program, call `yas-load-snippet-buffer'
and `kill-buffer' instead."
    (interactive (list (yas--read-table) nil))
    (yas-load-snippet-buffer table t)
    (noflet ((whitespace-cleanup (&rest _)))
      (cbyas:maybe-write-new-template yas--editing-template)
      (save-buffer)
      (quit-window t))))

;;; Utilities

(defmacro yas-with-field-restriction (&rest body)
  "Narrow the buffer to the current active field and execute BODY.
If no field is active, no narrowing will take place."
  (declare (indent 0))
  `(save-restriction
     (when (cbyas:current-field)
       (narrow-to-region (cbyas:beginning-of-field) (cbyas:end-of-field)))
     ,@body))

(defun cbyas:bol? ()
  "Non-nil if point is on an empty line or at the first word.
The rest of the line must be blank."
  (s-matches? (rx bol (* space) (* word) (* space) eol)
              (buffer-substring (line-beginning-position) (line-end-position))))

(defun cbyas:msg (fmt &rest args)
  "Like `message', but returns the empty string.
Embed in elisp blocks to trigger messages within snippets."
  (apply 'message (s-prepend "[yas] " fmt) args)
  "")

(defun yas-insert-first-snippet (predicate)
  "Choose a snippet to expand according to PREDICATE."
  (setq yas--condition-cache-timestamp (current-time))
  (let ((yas-buffer-local-condition 'always))
    (-if-let (yas--current-template
              (-first predicate (yas--all-templates (yas--get-snippet-tables))))
        (let ((where (if (region-active-p)
                         (cons (region-beginning) (region-end))
                       (cons (point) (point)))))
          (yas-expand-snippet (yas--template-content yas--current-template)
                              (car where)
                              (cdr where)
                              (yas--template-expand-env yas--current-template)))
      (error "No snippet matching predicate"))))

(defun cbyas:current-field ()
  "Return the current active field."
  (and (boundp 'yas--active-field-overlay)
       yas--active-field-overlay
       (overlay-buffer yas--active-field-overlay)
       (overlay-get yas--active-field-overlay 'yas--field)))

(defun cbyas:beginning-of-field ()
  (-when-let (field (cbyas:current-field))
    (marker-position (yas--field-start field))))

(defun cbyas:end-of-field ()
  (-when-let (field (cbyas:current-field))
    (marker-position (yas--field-end field))))

(defun cbyas:current-field-text ()
  "Return the text in the active snippet field."
  (-when-let (field (cbyas:current-field))
    (yas--field-text-for-display field)))

(defun cbyas:clear-blank-field ()
  "Clear the current field if it is blank."
  (-when-let* ((beg (cbyas:beginning-of-field))
               (end (cbyas:end-of-field))
               (str (cbyas:current-field-text)))
    (when (s-matches? (rx bos (+ space) eos) str)
      (delete-region beg end)
      t)))

(defun cbyas:maybe-goto-field-end ()
  "Move to the end of the current field if it has been modified."
  (-when-let (field (cbyas:current-field))
    (when (and (yas--field-modified-p field)
               (yas--field-contains-point-p field))
      (goto-char (cbyas:end-of-field)))))

;;; Advise editing commands.
;;;
;;; Pressing SPC in an unmodified field will clear it and switch to the next.
;;;
;;; Pressing S-TAB to go to last field will place point at the end of the field.

(defadvice yas-next-field (before clear-blank-field activate)
  (cbyas:clear-blank-field))

(defadvice yas-prev-field (before clear-blank-field activate)
  (cbyas:clear-blank-field))

(defadvice yas-next-field (after goto-field-end activate)
  (cbyas:maybe-goto-field-end))

(defadvice yas-prev-field (after goto-field-end activate)
  (cbyas:maybe-goto-field-end))

;;; Commands

(defun cbyas:reload-all ()
  (interactive)
  (yas-recompile-all)
  (yas-reload-all))

(defun cbyas:space ()
  "Clear and skip this field if it is unmodified. Otherwise insert a space."
  (interactive "*")
  (let ((field (cbyas:current-field)))
    (cond ((and field
                (not (yas--field-modified-p field))
                (eq (point) (marker-position (yas--field-start field))))
           (yas--skip-and-clear field)
           (yas-next-field 1))
          (t
           (insert " ")))))

(defun cbyas:backspace ()
  "Clear the current field if the current snippet is unmodified.
Otherwise delete backwards."
  (interactive "*")
  (let ((field (cbyas:current-field)))
    (cond ((and field
                (not (yas--field-modified-p field))
                (eq (point) (marker-position (yas--field-start field))))
           (yas--skip-and-clear field)
           (yas-next-field 1))
          ((true? smartparens-mode)
           (call-interactively 'sp-backward-delete-char))
          (t
           (call-interactively 'backward-delete-char)))))

;;; Key bindings

(bind-key "<backspace>" 'cbyas:backspace yas-keymap)
(bind-key "SPC" 'cbyas:space yas-keymap)

(provide 'config-yasnippet)

;;; config-yasnippet.el ends here
