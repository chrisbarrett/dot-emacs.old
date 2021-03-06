;;; config-modeline.el --- Customise the modeline

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

;; Customise the modeline

;;; Code:

(require 'config-theme)
(require 'config-orgmode)

(defface mode-line-tramp-separator
  '((((background dark))
     (:foreground "gray45"))
    (((background light))
     (:foreground "gray80"))
    (t
     (:inherit 'mode-line)))
  "Face for separator characters in modeline."
  :group 'modeline)

(defface mode-line-tramp-method
  '((t (:inherit 'mode-line)))
  "Face for tramp method in modeline."
  :group 'modeline)

(defface mode-line-tramp-user
  '((t (:foreground "VioletRed3" :inherit 'mode-line)))
  "Face for tramp user indicator in modeline."
  :group 'modeline)

(defface modeline-vc-unknown-face
  '((((background dark))
     (:foreground "yellow"))
    (((background light))
     (:foreground "blue"))
    (t
     (:inherit 'mode-line)))
  "Face for unknown vc file status."
  :group 'modeline)

(defface mode-line-read-only
  '((t (:foreground "#4271ae"
                    :box '(:line-width 2 :color "#4271ae")
                    :inherit 'mode-line)))
  "Face for readonly indicator."
  :group 'modeline)

(defface mode-line-modified
  '((t (:foreground "#c82829" :inherit 'mode-line)))
  "Face for modified indicator."
  :group 'modeline)

(defface mode-line-directory
  '((((background dark))
     (:foreground "gray60"))
    (((background light))
     (:foreground "gray70"))
    (t
     (:inherit 'mode-line)))
  "Face for the directory component of the current filename."
  :group 'modeline)

(defface mode-line-filename
  '((((background light))
     (:foreground "darkgoldenrod4" :weight bold))
    (((background dark))
     (:foreground "#eab700" :weight bold))
    (t
     (:inherit 'mode-line)))
  "Face for the name component of the current filename."
  :group 'modeline)

(defface mode-line-position
  `((((background dark))
     (:family ,(monospace-font)
              :height 100
              :foreground "gray60"))
    (((background light))
     (:family ,(monospace-font)
              :height 100
              :foreground "gray50"))
    (t
     (:inherit 'mode-line)))
  "Face for the position indicators."
  :group 'modeline)

(defface mode-line-mode
  '((((background dark))
     (:foreground "gray70"))
    (((background light))
     (:foreground "gray40"))
    (t
     (:inherit 'mode-line)))
  "Face for the current major mode indicator."
  :group 'modeline)

(defface mode-line-dim
  '((((background dark))
     (:foreground "gray40" :height 110))
    (((background light))
     (:foreground "gray70" :height 110))
    (t (:inherit 'mode-line-mode)))
  "Face for the current minor mode indicators."
  :group 'modeline)

(defface mode-line-process
  '((t (:foreground "#718c00" :inherit 'mode-line)))
  "Face for the current process."
  :group 'modeline)

(defface mode-line-80col
  '((((background dark))
     (:foreground "#eab700"))
    (((background light))
     (:foreground "#b58900"))
    (t
     (:inherit 'mode-line-position)))
  "Face for the warning when point is past column 80."
  :group 'modeline)

(cl-defun cb:vc-state->letter (&optional (file (buffer-file-name)))
  "Return a single letter to represent the current version-control status."
  (cl-case (ignore-errors (vc-state file))
    ((up-to-date)           " ")
    ((edited)               (propertize "M" 'face '(:foreground "red")))
    ((needs-merge conflict) (propertize "!" 'face '(:foreground "red")))
    ((added)                (propertize "A" 'face '(:foreground "green")))
    ((removed)              (propertize "D" 'face '(:foreground "red")))
    ((ignored)              (propertize "-" 'face 'modeline-vc-unknown-face))
    (t                      (propertize "?" 'face 'modeline-vc-unknown-face))))

(cl-defun cb:shorten-directory (dir &optional (max-length 15))
  "Show up to MAX-LENGTH characters of a directory name DIR."
  (let ((dir (abbreviate-file-name dir)))
    (if (< (length dir) max-length)
        dir
      (->> (split-string dir (f-path-separator))
        (--map (-take 2 (string-to-list it)))
        (s-join (f-path-separator))))))

(autoload 'tramp-dissect-file-name "tramp")

(cl-defun cb:propertize-file-directory
    (&optional (filepath (file-name-directory (buffer-file-name))))
  "Separate tramp info from the given filepath."
  (cl-flet ((face
             (str face)
             (propertize str 'face face)))

    (cl-destructuring-bind (&optional method user host file &rest rs)
        (mapcar 'identity (ignore-errors
                            (tramp-dissect-file-name filepath)))
      (concat
       (when host
         (concat
          (face "/" 'mode-line-tramp-separator)
          (face method 'mode-line-tramp-method)
          (face ":" 'mode-line-tramp-separator)
          (face user 'mode-line-tramp-user)
          (face "@" 'mode-line-tramp-separator)
          host
          (face ":" 'mode-line-tramp-separator)))
       (face (cb:shorten-directory (or file filepath)) 'mode-line-directory)))))

(defun cbmd:description ()
  "Format the mode line description for this buffer."
  (concat
   (if (buffer-file-name) (cb:propertize-file-directory) "")
   (propertize (buffer-name) 'face 'mode-line-filename)))

(defun cbmd:mode-line-fill (reserve)
  "Return empty space and leave RESERVE space on the right."
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

(defun cbmd:rightmost-window? (win)
  "Non-nil if WIN is the right-most window in this frame."
  (let ((x-max (frame-width))
        (y-max (- (frame-height) (window-height (minibuffer-window)) 1)))
    (equal win (window-at x-max y-max))))

(defun cbmd:pomodoro-string ()
  "Format the pomodoro string for the modeline."
  (cond
   ;; Show the current pomodoro time and an indicator for the length of the
   ;; next break.
   ((and (true? org-pomodoro-mode-line)
         (equal org-pomodoro-state :pomodoro))

    (cl-destructuring-bind (bl s br)
        org-pomodoro-mode-line
      (let ((upcoming-long-break?
             (and (plusp org-pomodoro-count) ; starts at 0
                  (zerop (mod org-pomodoro-count
                              org-pomodoro-long-break-frequency)))))
        (concat bl s
                " → " (if upcoming-long-break? "L" "S")
                br
                " "))))

   ;; Show just the time when on a break.
   ((and (true? org-pomodoro-mode-line)
         (concat org-pomodoro-mode-line)))))

(defun cbmd:frame-maximised? (&optional frame)
  "Non-nil if FRAME is maximised."
  (memq (frame-parameter frame 'fullscreen) '(fullscreen fullboth)))

(defun cbmd:date-and-time-string ()
  "Format date and time string for modeline."
  (propertize (format-time-string " %a %e %b %R") 'face 'mode-line-process))

(defun cbmd:file-status-string ()
  "Format modeline VC state for current file."
  (let ((blank "    "))
    (cond
     ;; Do not show status for special buffers.
     ((and (s-starts-with? "*" (buffer-name))
           (not (buffer-file-name)))
      blank)

     ;; Show read-only indicator.
     (buffer-read-only
      (propertize " RO " 'face 'mode-line-read-only))

     ;; Show modified and vc status.
     (t
      (format " %s%s "
              (if (ignore-errors (vc-git-root (buffer-file-name)))
                  (cb:vc-state->letter)
                " ")
              (if (buffer-modified-p)
                  (propertize "*" 'face 'mode-line-modified)
                " "))))))

(defun cbmd:column-number-string ()
  (propertize "%3c" 'face
              (if (>= (current-column) 80)
                  'mode-line-80col
                'mode-line-position)))

(defun cbmd:right-widgets ()
  "Make a string for widgets at the far right side of the modeline."
  (let* ((gms (s-join " " (-map 'eval global-mode-string)))
         (str (concat gms
                      " "
                      (cbmd:pomodoro-string)
                      (when (cbmd:frame-maximised?) (cbmd:date-and-time-string))
                      " "))
         (str (concat (cbmd:mode-line-fill (length str)) str)))
    (cond
     ((and (cbmd:frame-maximised?) (cbmd:rightmost-window? (selected-window)))
      str)

     ((and (not (cbmd:frame-maximised?))
           (cbmd:rightmost-window? (selected-window)))
      str))))

(setq-default
 mode-line-format
 `(
   ;; Line and column number.
   (:propertize " L" face mode-line-dim)
   (:propertize "%l %p:" face mode-line-position)
   (:eval (cbmd:column-number-string))
   " "
   ;; Evil state
   (:eval (if (and (featurep 'evil) (true? evil-mode)) evil-mode-line-tag ""))

   ;; File status.
   (:eval (cbmd:file-status-string))
   " "
   ;; Buffer name and path.
   (:eval (cbmd:description))

   ;; Narrowing
   " %n "

   ;; Major mode
   " %[" (:propertize mode-name face mode-line-mode) "%] "

   ;; Minor modes
   (:eval (propertize (format-mode-line minor-mode-alist) 'face 'mode-line-dim))
   (:propertize mode-line-process face mode-line-process)

   ;; Right-side widgets
   (:eval (cbmd:right-widgets))))

(defvar cb:modeline-timer
  (run-with-timer 5 5 (lambda ()
                        (dolist (_ (buffer-list))
                          (when (and (buffer-file-name)
                                     (projectile-project-p)
                                     (projectile-project-vcs))
                            (vc-after-save)
                            (force-mode-line-update)))))
  "Timer that updates the modeline to show current VC state.")

(provide 'config-modeline)

;;; config-modeline.el ends here
