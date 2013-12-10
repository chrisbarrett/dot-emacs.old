;;; cb-file-picker-widget.el --- Provides a file picker widget.

;; Copyright (C) 2013 Chris Barrett

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

;; Provides a file picker widget. See the docstring for `file-picker'.

;;; Code:

(require 'cb-lib)
(require 'cb-colour)
(autoload 'cl-gensym "cl-macs")

(defgroup file-picker-widget nil
  "Customisations for the file picker widget."
  :group 'widgets
  :prefix "file-picker")

(defface file-picker-header-line
  `((t :foreground ,solarized-hl-yellow :bold t))
  "Face used for headers in a file picker."
  :group 'file-picker-widget)

(defvar file-picker-mode-map
  (let ((km (make-sparse-keymap)))
    ;; Navigation
    (define-key km (kbd "j") 'file-picker-next-file)
    (define-key km (kbd "k") 'file-picker-previous-file)
    (define-key km (kbd "n") 'file-picker-next-file)
    (define-key km (kbd "p") 'file-picker-previous-file)
    (define-key km [down] 'file-picker-next-file)
    (define-key km [up] 'file-picker-previous-file)
    ;; Editing
    (define-key km (kbd "a") 'file-picker-append-file)
    (define-key km (kbd "c") 'file-picker-clear)
    (define-key km (kbd "d") 'file-picker-remove-file)
    (define-key km (kbd "<backspace>") 'file-picker-remove-file)
    (define-key km (kbd "g") 'file-picker-append-glob)
    (define-key km (kbd "RET") 'file-picker-show-file)
    (define-key km (kbd "C-c C-k") 'file-picker-abort)
    (define-key km (kbd "C-c C-c") 'file-picker-accept)
    ;; Undo
    (define-key km (kbd "u") 'file-picker-undo)
    (define-key km (kbd "C-r") 'file-picker-redo)
    ;; Structure
    (define-key km (kbd "M-<up>") 'file-picker-move-file-up)
    (define-key km (kbd "M-<down>") 'file-picker-move-file-down)
    km))

(defvar-local file-picker-accept-function nil
  "A unary handler function taking the list of files selected by
  the user in a file picker.")

(defvar-local file-picker-window-register nil
  "Register symbol for restoring the window configuration to its
state before the file picker was shown.")

(defvar-local file-picker-last-directory nil
  "Used to make file reader commands remember the last directory
read from the user.")

(defun file-picker-pp-option (key desc)
  "Propertize a file picker key command for display in the key summary.
KEY and DESC are the key binding and command description."
  (concat "[" (propertize key 'face 'option-key) "] " desc))

(defun file-picker-format-info ()
  (propertize "Select the files to act on. Use M-up and M-down reorder the list."
              'face 'font-lock-comment-face))

(defun file-picker-format-key-summary ()
  "Format a string listing available key commands."
  (let* ((cmds (-map (@ 'file-picker-pp-option)
                     '(("a" "Add File")
                       ("g" "Add Files (Glob)")
                       ("d" "Remove File")
                       ("c" "Clear")
                       ("C-c C-c" "Accept")
                       ("C-c C-k" "Abort"))))
         (max-width (-max (-map 'length cmds))))
    (concat
     (propertize "Commands:\n" 'face 'file-picker-header-line)
     (->> cmds
       (AP (<> cb-lib:columnate-lines) (+ 4 max-width))
       (s-split "\n")
       (-map (~ s-prepend "    "))
       (s-join "\n")))))

(defun file-picker-format-files-header ()
  (propertize "Selected Files:" 'face 'file-picker-header-line))

(defun file-picker-files ()
  "Get the list of files added to the file picker."
  (save-excursion
    (file-picker-goto-files)
    (let ((section (s-trim (buffer-substring (point) (point-max)))))
      (unless (s-blank? section)
        (-map (C f-expand s-trim) (s-split "\n" section))))))

(defun file-picker-clear ()
  "Remove all files in the file picker."
  (interactive)
  (atomic-change-group
    ;; Ignore if empty.
    (if (null (file-picker-files))
        (when (called-interactively-p nil)
          (user-error "List is empty"))

      ;; Prompt user to confirm.
      (when (called-interactively-p)
        (unless (y-or-n-p "Clear all files? ")
          (user-error "Cancelled")))

      ;; Clear files list.
      (let (buffer-read-only)
        (file-picker-goto-files)
        (delete-region (point) (point-max))
        (insert "    "))

      (when (called-interactively-p nil)
        (message "List cleared")))))

(defun file-picker-accept ()
  "Accept the files and signal input is finished.
Sends a 'files-accepted signal with the list of file paths as data.
The signal is captured by the event loop in `file-picker'."
  (interactive)
  (let ((files (file-picker-files))
        (buf (current-buffer)))
    (file-picker-restore-previous-window-state)
    (with-current-buffer buf
      (unwind-protect (funcall file-picker-accept-function files)
        (kill-buffer buf)))))

(defun file-picker-abort ()
  "Close the current file-picker and signal an error."
  (interactive)
  (let ((buf (current-buffer)))
    (file-picker-restore-previous-window-state)
    (kill-buffer buf))

  (user-error "Aborted"))

(defun file-picker-goto-files ()
  "Move to the files section of a file picker."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp (rx bol "Selected Files:" (* space) eol))
  (forward-line)
  (goto-char (line-beginning-position))
  (point))

(defun file-picker-in-file-section? ()
  "Non-nil if point is in the file section of a file picker."
  (-when-let (file-section (save-excursion (file-picker-goto-files)))
    (>= (point) file-section)))

(defun file-picker-remove-file ()
  "Delete the filepath at point in a file picker."
  (interactive)
  (atomic-change-group
    (let ((indented-line? (s-matches? (rx "    " (+ nonl)) (current-line))))
      (cond ((and (file-picker-in-file-section?) indented-line?)

             (let (buffer-read-only)
               (delete-region (line-beginning-position) (line-end-position))
               ;; Don't join with the section header if we just deleted the last file.
               (when (file-picker-files)
                 (join-line)
                 (forward-line)))

             (goto-char (line-beginning-position)))
            ((and (file-picker-in-file-section?)
                  (null (file-picker-files)))
             (error "List is empty"))
            (t
             (error "Point is not at a file"))))))

(defun file-picker-append-file (path)
  "Add PATH to the current file picker selection."
  (interactive (list (ido-read-file-name "Add File: "
                                         file-picker-last-directory)))

  (setq file-picker-last-directory (f-dirname path))
  (atomic-change-group
    (let ((line (concat "    " (f-short (s-trim path)))))
      (goto-char (point-max))

      (let (buffer-read-only)
        (while (s-matches? (rx bol (* space) eol) (current-line))
          (join-line))
        (newline)
        (insert (propertize line 'face 'font-lock-string-face)))

      (goto-char (line-beginning-position)))))

(defun file-picker-append-glob (glob)
  "Add multiple files matching GLOB pattern to a file picker."
  (interactive (list (read-file-name "Glob: "
                                     file-picker-last-directory)))
  (setq file-picker-last-directory (f-dirname glob))
  (atomic-change-group
    (-each (file-expand-wildcards glob t) 'file-picker-append-file)))

(defun file-picker-eob? ()
  (and (file-picker-in-file-section?)
       (or (= (line-number-at-pos) (line-number-at-pos (point-max)))
           (s-blank? (save-excursion (forward-line) (current-line))))))

(defun file-picker-move-file-down ()
  "Move the current file down in the file picker list."
  (interactive)
  (cond
   ((or (not (file-picker-in-file-section?))
        (s-blank? (s-trim (current-line))))
    (error "Point is not at a file"))
   ((file-picker-eob?)
    (error "End of section"))
   (t
    (atomic-change-group
      (forward-line)

      (let (buffer-read-only)
        (file-picker-move-file-up))

      (forward-line)))))

(defun file-picker-move-file-up ()
  "Move the current file up in the file picker list."
  (interactive)
  (cond
   ((or (not (file-picker-in-file-section?))
        (s-blank? (s-trim (current-line))))
    (error "Point is not at a file"))
   ((not (save-excursion
           (forward-line -1)
           (file-picker-in-file-section?)))
    (error "Start of section"))
   (t
    (atomic-change-group

      (let (buffer-read-only)
        (transpose-lines 1))

      (forward-line -2)))))

(defun file-picker-next-file ()
  "Move to the next file in the file picker."
  (interactive)
  (cond
   ((not (file-picker-in-file-section?))
    (file-picker-goto-files))
   ((not (file-picker-eob?))
    (forward-line 1)
    (goto-char (line-beginning-position)))))

(defun file-picker-previous-file ()
  "Move to the previous file in the file picker."
  (interactive)
  (cond
   ((not (file-picker-in-file-section?))
    (file-picker-goto-files))

   ((save-excursion
      (forward-line -1)
      (file-picker-in-file-section?))
    (forward-line -1)
    (goto-char (line-beginning-position)))))

(defun file-picker-show-file (filename)
  "Show FILENAME in another frame."
  (interactive (list (s-trim (current-line))))
  (if (file-picker-in-file-section?)
      (find-file-other-window filename)
    (user-error "Point is not at a file")))

(defun file-picker-undo (arg)
  (interactive "P")
  (let (buffer-read-only)
    (undo-tree-undo arg)))

(defun file-picker-redo (arg)
  (interactive "P")
  (let (buffer-read-only)
    (undo-tree-redo arg)))

(defun file-picker-restore-previous-window-state ()
  "Restore window state to that prior to when the file picker was shown."
  (jump-to-register file-picker-window-register))

(define-derived-mode file-picker-mode nil "FilePicker"
  "Major mode for interactively selecting a number of files.
\\{file-picker-mode-map}"
  (setq-local require-final-newline nil)
  (when (fboundp 'evil-emacs-state)
    (evil-emacs-state)))

;;;###autoload
(cl-defmacro file-picker (title &rest keys)
  "Show a file picker widget with TITLE.
The picker allows the user to input a number of files.

* ON-ACCEPT is a unary function. It will be called with the list of files once
  the user has finished.

* DEFAULT-DIR is the default directory to use when reading files.

\(fn title &key on-accept [default-dir])"
  (let ((on-accept (plist-get keys :on-accept))
        (default-dir (plist-get keys :default-dir)))
    (cl-assert on-accept nil "Must provide :on-accept function")
    (cl-assert (cl-evenp (length keys)))
    (let ((keys (-map 'car (-partition-in-steps 2 2 keys))))
      (cl-assert (null (-difference keys '(:on-accept :default-dir)))))
    `(progn
       (let ((register (cl-gensym)))
         ;; Save current window configuration.
         (window-configuration-to-register register)
         (switch-to-buffer (get-buffer-create ,title))
         (file-picker-mode)
         (delete-other-windows)

         (setq file-picker-window-register register
               file-picker-accept-function ,on-accept
               file-picker-last-directory ,default-dir))

       ;; Insert description and sections.
       (erase-buffer)
       (insert (file-picker-format-info))
       (newline 2)
       (insert (file-picker-format-key-summary))
       (newline 2)
       (insert (file-picker-format-files-header))
       (newline)

       (read-only-mode +1)

       ;; Start the undo history from this point.
       (setq buffer-undo-list nil
             buffer-undo-tree nil))))

(provide 'cb-file-picker-widget)

;;; cb-file-picker-widget.el ends here
