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

;;; Buffers

(defun cb:larger-window (win1 win2)
  (let ((x1 (window-width win1))
        (x2 (window-width win2))
        (y1 (window-height win1))
        (y2 (window-height win2)))
    ;; Select tallest if same width.
    (if (= x1 x2)
        (if (> y1 y2) win1 win2)
      ;; Select widest.
      (if (> x1 x2) win1 win2))))

;;;###autoload
(defun cb:select-largest-window ()
  (interactive)
  (select-window (-reduce 'cb:larger-window (window-list))))

;;;###autoload
(defun cb:rotate-buffers ()
  "Rotate active buffers, retaining the window layout. Changes
the selected buffer."
  (interactive)
  (cond
   ((not (> (count-windows) 1))
    (message "You can't rotate a single window!"))
   (t
    (let ((i 1)
          (numWindows (count-windows)))
      (while  (< i numWindows)
        (let* (
               (w1 (elt (window-list) i))
               (w2 (elt (window-list) (+ (% i numWindows) 1)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2))
               )
          (set-window-buffer w1  b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i))))))))

(defvar cb:kill-buffer-ignored-list nil)

;;;###autoload
(defun cb:kill-current-buffer ()
  "Kill the current buffer.
If this buffer is a member of `kill-buffer-ignored-list, bury it rather than killing it."
  (interactive)
  (if (member (buffer-name (current-buffer)) cb:kill-buffer-ignored-list)
      (bury-buffer)
    (kill-buffer (current-buffer))))

;;;###autoload
(defun cb:hide-dos-eol ()
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M [])
  (aset buffer-display-table ?\^L []))

;;;###autoload
(cl-defun cb:last-buffer-for-mode (mode &optional (buffers (buffer-list)))
  "Return the previous buffer with major mode MODE."
  (--first (with-current-buffer it
             (equal mode major-mode))
           buffers))

(defun cb:timestamp ()
  (format-time-string "%Y%m%d.%H%M" nil t))

;;;###autoload
(defun cb:insert-timestamp ()
  "Insert a package-conformant timestamp, of the format YYYYMMDD.HHMM at point."
  (interactive)
  (insert (cb:timestamp)))

;;;###autoload
(defun cb:indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

;;;###autoload
(defun cb:indent-dwim ()
  "Perform a context-sensitive indentation action."
  (interactive)
  (cond ((region-active-p)
         (indent-region (region-beginning) (region-end)))

        ((-contains? '(font-lock-comment-face
                       font-lock-string-face
                       font-lock-doc-string-face)
                     (face-at-point))
         (fill-paragraph))

        ((thing-at-point 'defun)
         (save-excursion
           (mark-defun)
           (indent-region (region-beginning) (region-end))))
        (t
         (cb:indent-buffer))))

;;;###autoload
(defun cb:rename-file-and-buffer ()
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

(defun cb:find-autoloads (buffer)
  (->> (with-current-buffer buffer
         (buffer-substring-no-properties (point-min) (point-max)))
    (s-match-strings-all (rx ";;;###autoload\n(" (+ (not space)) (+ space) (? "'")
                             (group (+ (not space)))))
    (-map 'cadr)))

;;;###autoload
(cl-defun cb:show-autoloads (&optional (buffer (current-buffer)))
  "Find the autoloaded definitions in BUFFER"
  (interactive)
  (-if-let (results (--map (s-append "\n" it)
                           (cb:find-autoloads buffer)))
    (with-output-to-temp-buffer "*autoloads*"
      (-each results 'princ))

    (error "No autoloads found in current buffer")))

(provide 'cb-commands)

;;; cb-commands.el ends here
