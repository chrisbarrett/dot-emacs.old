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

;; Provides a file picker widget.

;;; Code:

(require 'cb-lib)

(defvar-local file-picker-accept-function 'ignore
  "A unary handler function taking the list of files selected by
  the user in a file picker.")

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
    (define-key km (kbd "g") 'file-picker-append-glob)
    (define-key km (kbd "C-c C-k") 'file-picker-abort)
    (define-key km (kbd "C-c C-c") 'file-picker-accept)
    ;; Structure
    (define-key km (kbd "M-<up>") 'file-picker-move-file-up)
    (define-key km (kbd "M-<down>") 'file-picker-move-file-down)
    km))

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
     "Commands:\n"
     (->> cmds
       (AP (<> cb-lib:columnate-lines) (+ 4 max-width))
       (s-split "\n")
       (-map (~ s-prepend "    "))
       (s-join "\n")))))

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
  ;; Ignore if empty.
  (if (null (file-picker-files))
      (when (called-interactively-p)
        (user-error "List is empty"))

    ;; Prompt user to confirm.
    (when (called-interactively-p)
      (unless (y-or-n-p "Clear all files? ")
        (user-error "Cancelled")))

    ;; Clear files list.
    (file-picker-goto-files)
    (let (buffer-read-only)
      (delete-region (point) (point-max))
      (insert "    "))

    (when (called-interactively-p)
      (message "List cleared"))))

(defun file-picker-accept ()
  "Accept the files and signal input is finished.
Sends a 'files-accepted signal with the list of file paths as data.
The signal is captured by the event loop in `file-picker'."
  (interactive)
  (let ((files (file-picker-files)))
    (kill-buffer)
    (funcall file-picker-accept-function files)))

(defun file-picker-abort ()
  "Close the current file-picker and signal an error."
  (interactive)
  (kill-buffer)
  (user-error "Aborted"))

(defun file-picker-goto-files ()
  "Move to the files section of a file picker."
  (interactive)
  (goto-char (point-min))
  (when (search-forward-regexp (rx bol "Selected Files:" eol) nil t)
    (forward-line)
    (point)))

(defun file-picker-in-file-section? ()
  "Non-nil if point is in the file section of a file picker."
  (-when-let (file-section (save-excursion (file-picker-goto-files)))
    (>= (point) file-section)))

(defun file-picker-remove-file ()
  "Delete the filepath at point in a file picker."
  (interactive)
  (let (buffer-read-only)
    (let ((indented-line? (s-matches? (rx "    " (+ nonl)) (current-line))))
      (cond ((and (file-picker-in-file-section?) indented-line?)
             (delete-region (line-beginning-position) (line-end-position))
             (join-line))
            (t
             (error "Point is not at a file"))))))

(defun file-picker-append-file (path)
  "Add PATH to the current file picker selection."
  (interactive (list (ido-read-file-name "Add File: ")))
  (let (buffer-read-only
        (line (concat "    " (f-short (s-trim path)))))
    (goto-char (point-max))
    (while (s-matches? (rx bol (* space) eol) (current-line))
      (join-line))
    (newline)
    (insert (propertize line 'face 'font-lock-string-face))))

(defun file-picker-append-glob (glob)
  "Add multiple files matching GLOB pattern to a file picker."
  (interactive (list (read-file-name "Glob: ")))
  (-each (file-expand-wildcards glob t) 'file-picker-append-file))

(defun file-picker-eob? ()
  (and (file-picker-in-file-section?)
       (or (= (line-number-at-pos) (line-number-at-pos (point-max)))
           (s-blank? (save-excursion (forward-line) (current-line))))))

(defun file-picker-move-file-down ()
  "Move the current file down in the file picker list."
  (interactive)
  (cond
   ((not (file-picker-in-file-section?))
    (error "Point is not at a file"))
   ((file-picker-eob?)
    (error "End of section"))
   (t
    (forward-line)
    (file-picker-move-file-up)
    (forward-line))))

(defun file-picker-move-file-up ()
  "Move the current file up in the file picker list."
  (interactive)
  (cond
   ((not (file-picker-in-file-section?))
    (error "Point is not at a file"))
   ((not (save-excursion
           (forward-line -1)
           (file-picker-in-file-section?)))
    (error "Start of section"))
   (t
    (let (buffer-read-only)
      (transpose-lines 1)
      (forward-line -2)))))

(defun file-picker-next-file ()
  "Move to the next file in the file picker."
  (interactive)
  (cond
   ((not (file-picker-in-file-section?))
    (file-picker-goto-files))
   ((not (file-picker-eob?))
    (forward-line 1))))

(defun file-picker-previous-file ()
  "Move to the previous file in the file picker."
  (interactive)
  (cond
   ((not (file-picker-in-file-section?))
    (file-picker-goto-files))

   ((save-excursion
      (forward-line -1)
      (file-picker-in-file-section?))
    (forward-line -1))))

(define-derived-mode file-picker-mode fundamental-mode "FilePicker"
  "Major mode for interactively selecting a number of files."
  (setq-local require-final-newline nil))

;;;###autoload
(cl-defun file-picker (title &key on-accept)
  "Show a file picker widget with TITLE.
The picker allows the user to input a number of files.

* ON-ACCEPT is a unary function. It will be called with the list of files once
  the user has finished."
  ;; Prepare buffer.
  (switch-to-buffer (get-buffer-create title))
  (delete-other-windows)
  (read-only-mode +1)
  (setq-local file-picker-accept-function on-accept)

  (let (buffer-read-only)
    (file-picker-mode)
    (erase-buffer)

    ;; Insert section skeleton.
    (insert (file-picker-format-info))
    (newline 2)
    (insert (file-picker-format-key-summary))
    (insert "\n\nSelected Files:\n    ")

    (when (fboundp 'evil-emacs-state)
      (evil-emacs-state))))

(provide 'cb-file-picker-widget)

;;; cb-file-picker-widget.el ends here