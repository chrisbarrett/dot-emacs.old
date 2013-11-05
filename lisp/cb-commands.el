;;; cb-commands --- Basic configuration

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

;; Basic configuration required for a sane editing environment.

;;; Code:

(require 'dash)
(require 's)
(require 'bind-key)
(require 'cb-lib)
(require 'cb-mode-groups)
(autoload 'emr-reporting-buffer-changes "emr")
(autoload 'org-move-item-down "org-list")
(autoload 'org-move-item-up "org-list")

;;; Buffers

;;;###autoload
(defun cb:rotate-buffers ()
  "Rotate active buffers, retaining the window layout.
Changes the selected buffer."
  (interactive)
  ;; Bail if there are not enough windows to rotate.
  (unless (> (count-windows) 1)
    (user-error "Cannot rotate single window"))
  ;; Perform rotation.
  (let ((i 1)
        (n-windows (count-windows)))
    (while  (< i n-windows)
      (let* (
             (w1 (elt (window-list) i))
             (w2 (elt (window-list) (+ (% i n-windows) 1)))
             (b1 (window-buffer w1))
             (b2 (window-buffer w2))
             (s1 (window-start w1))
             (s2 (window-start w2))
             )
        (set-window-buffer w1  b2)
        (set-window-buffer w2 b1)
        (set-window-start w1 s2)
        (set-window-start w2 s1)
        (setq i (1+ i))))))

(defvar cb:kill-buffer-ignored-list
  '("*scratch*" "*Messages*" "*Group*"
    "*shell*" "*eshell*" "*ansi-term*"
    "diary.org" "notes.org"))

(defun kill-current-buffer ()
  "Kill the current buffer.
If this buffer is a member of `cb:kill-buffer-ignored-list, bury it rather than killing it."
  (interactive)
  (if (member (buffer-name (current-buffer)) cb:kill-buffer-ignored-list)
      (bury-buffer)
    (kill-buffer (current-buffer))))

(defun clean-buffers ()
  "Close all buffers not in the ignore list."
  (interactive)
  (delete-other-windows)
  (-each (--filter-buffers
          (not (or (-contains? cb:kill-buffer-ignored-list (buffer-name it))
                   (get-buffer-process it))))
         'kill-buffer))

(defun insert-timestamp ()
  "Insert a package-conformant cb:timestamp, of the format YYYYMMDD.HHMM at point."
  (interactive)
  (insert (format-time-string "%Y%m%d.%H%M" nil t)))

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (ignore-errors
    (save-excursion
     (indent-region (point-min) (point-max)))))

(defun indent-dwim (&optional arg)
  "Perform a context-sensitive indentation action.
With prefix argument ARG, justify text."
  (interactive "P")
  (cond
   ((region-active-p)
    (indent-region (region-beginning) (region-end))
    (message "Indented region."))

   ((-contains? '(font-lock-comment-face
                  font-lock-string-face
                  font-lock-doc-face)
                (face-at-point))
    (if (apply 'derived-mode-p cb:lisp-modes)
        (lisp-fill-paragraph arg)
      (fill-paragraph arg))
    (message "Filled paragraph."))

   ((thing-at-point 'defun)
    (indent-region
     (save-excursion (beginning-of-defun) (point))
     (save-excursion (end-of-defun) (point)))
    (message "Indented defun."))

   (t
    (indent-buffer)
    (message "Indented buffer."))))

(defun rename-buffer-and-file ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))

(defalias 'rename-file-and-buffer 'rename-buffer-and-file)

(defun delete-buffer-and-file ()
  "Delete a file and its associated buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defalias 'delete-file-and-buffer 'delete-buffer-and-file)

(defun cb:find-autoloads (buffer)
  (->> (with-current-buffer buffer
         (buffer-substring-no-properties (point-min) (point-max)))
    (s-match-strings-all (rx ";;;###autoload" "\n"
                             (* space) "("(+ (not space)) (+ space) (? "'")
                             (group (+ (not space)))))
    (-map 'cadr)))

(cl-defun show-autoloads (&optional (buffer (current-buffer)))
  "Find the autoloaded definitions in BUFFER"
  (interactive)
  (-if-let (results (-map (~ s-append "\n") (cb:find-autoloads buffer)))
    (with-output-to-temp-buffer "*autoloads*"
      (-each results 'princ))

    (error "No autoloads found in current buffer")))

;;; Shebang insertion

(defun cb:filename->interpreter (filename)
  (cdr
   (assoc (file-name-extension filename)
          '(("el" . "emacs")
            ("hs" . "runhaskell")
            ("py" . "python")
            ("rb" . "ruby")
            ("sh" . "bash")))))

(defun insert-shebang (cmd)
  "Insert a shebang line at the top of the current buffer.
Prompt for a command CMD if one cannot be guessed."
  (interactive
   (list (or (cb:filename->interpreter buffer-file-name)
          (read-string "Command name: " nil t))))
  (require 'emr)
  (emr-reporting-buffer-changes "Inserted shebang"
    (save-excursion
      (goto-char (point-min))
      (open-line 2)
      (insert (concat "#!/usr/bin/env " cmd)))))

(defun move-line-up ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-up)

    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode)))

(defun move-line-down ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-down)

    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode)))

(defun goto-first-occurence ()
  "Move to the first occurence the symbol at point."
  (interactive)
  (eval
   `(progn
      (goto-char (point-min))
      (search-forward-regexp
       (rx symbol-start ,(thing-at-point 'symbol) symbol-end))
      (beginning-of-thing 'symbol))))

(defun swap-with-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun insert-variable (variable)
  "Insert the value of VARIABLE at point."
  (interactive
   (list
    (intern
     (ido-completing-read
      "Variable: "
      (-map 'symbol-name
            (filter-atoms (-orfn 'custom-variable-p 'special-variable-p)))))))
  (insert (pp-to-string (eval variable))))

(defun insert-uuid ()
  "Insert a GUID at point."
  (interactive "*")
  (insert (make-uuid)))

(defalias 'insert-guid 'insert-uuid)

(defun insert-lorem-ipsum (n-paragraphs paragraph-length)
  "Insert N-PARAGRAPHS of lorem ipsum text into the current buffer.
PARAGRAPH-LENGTH is one of short, medium, long or verylong."
  (interactive
   (list (read-number "Number of paragraphs: " 3)
         (ido-completing-read "Paragraph length: "
                              '("short" "medium" "long" "verylong"))))
  (let ((url (format "http://loripsum.net/api/%s/%s/plaintext"
                     n-paragraphs paragraph-length)))
    (insert (with-current-buffer (url-retrieve-synchronously url)
              ;; Skip HTTP header.
              (goto-char (point-min))
              (search-forward "\n\n")
              (s-trim (buffer-substring (point) (point-max)))))))

(defun outdent ()
  "Remove indentation on the current line."
  (interactive "*")
  (save-excursion
    (goto-char (line-beginning-position))
    (delete-horizontal-space)))

(provide 'cb-commands)

;;; cb-commands.el ends here
