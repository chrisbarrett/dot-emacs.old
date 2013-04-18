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

(defun cb:select-largest-window ()
  (interactive)
  (select-window (-reduce 'cb:larger-window (window-list))))

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

(defvar cb:kill-buffer-ignored-list
  '("*scratch*" "*Messages*" "*GROUP*" "*shell*" "*eshell*"))

(defun cb:kill-current-buffer ()
  "Kill the current buffer.
If this buffer is a member of `kill-buffer-ignored-list, bury it rather than killing it."
  (interactive)
  (if (member (buffer-name (current-buffer)) cb:kill-buffer-ignored-list)
      (bury-buffer)
    (kill-buffer (current-buffer))))

(defun sudo-edit (&optional arg)
  "Edit a file with elevated privileges."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun cb:hide-dos-eol ()
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M [])
  (aset buffer-display-table ?\^L []))

(defun cb:last-buffer-for-mode (mode)
  "Return the previous buffer with major mode MODE."
  (->> (buffer-list) (--first (with-current-buffer it
                                (equal mode major-mode)))))

(defun cb:timestamp ()
  (format-time-string "%Y%m%d.%H%M" nil t))

(defun insert-timestamp ()
  "Insert a package-conformant timestamp, of the format YYYYMMDD.HHMM at point."
  (interactive)
  (insert (cb:timestamp)))

(provide 'cb-commands)

;;; cb-commands.el ends here
