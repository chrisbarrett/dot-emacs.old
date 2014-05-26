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

(defface mode-line-minor-mode
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

(defface modeline-org-notes-file-indicator
  `((t (:foreground
        ,solarized-hl-magenta
        :inherit
        'mode-line-position)))
  "Face for the indicator showing the name of the current org notes file."
  :group 'modeline)

(defvar modeline-mail-indicator nil)

(defvar modeline-custom-description-functions nil
  "A list of functions.
The first function returning non-nil is used for the description
section in the modeline.")

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

(cl-defun cb:vc-file-uptodate? (&optional (file (buffer-file-name)))
  "Non-nil if FILE is up-to-date."
  (ignore-errors
    (vc-state-refresh file 'git)
    (equal 'up-to-date (vc-state file))))

(cl-defun cb:shorten-directory (dir &optional (max-length 30))
  "Show up to MAX-LENGTH characters of a directory name DIR."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    ;; Ellipsize the path if it is too long.
    ;; `2` is the length of the path separator + ellipsis.
    (while (and path (< (length output) (- max-length 2)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

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
  "Format the mode line description.
This will normally be the path and buffer name, unless there is a suitable
entry in `modeline-custom-description-functions'."
  (or
   (-when-let (s (-first 'funcall modeline-custom-description-functions))
     (propertize (funcall s) 'face 'mode-line-filename))
   (concat
    (if (buffer-file-name) (cb:propertize-file-directory) "")
    (propertize (buffer-name) 'face 'mode-line-filename))))

(defvar cbm:mail-icon (create-image (f-join cb:assets-dir "letter.xpm")
                                    'xpm nil :ascent 'center))

(defun cbm:unread-mail-count ()
  "Return the number of unread messages in all folders in your maildir."
  (->> (f-directories user-mail-directory)
    (-mapcat 'f-directories)
    (-mapcat 'f-directories)
    (-filter (~ s-ends-with? "new"))
    (-remove (~ s-matches? (rx (or "low" "archive" "draft" "org"
                                   "deleted" "trash" "sent"))))
    (-map (C length f-files))
    (-sum)))

(defvar cbm:mode-line-indicator nil
  "The entry to display in the modeline.")

(defun cbm:make-indicator (n)
  (when (cl-plusp n)
    (concat
     (propertize "@" 'display cbm:mail-icon)
     (int-to-string n))))

(defun cbm:update-unread-count ()
  "Find the number of unread messages and update the modeline."
  (when (f-exists? user-mail-directory)
    (setq cbm:mode-line-indicator (cbm:make-indicator (cbm:unread-mail-count)))))

(defvar cbm:unread-count-timer
  (run-with-timer 0 10 'cbm:update-unread-count))

(setq-default
 mode-line-format
 `(
   ;; --------------------------------------------------------------------------
   ;; Line and column number.
   (:propertize " %4l:" face mode-line-position)
   (:eval
    ;; Warn if over 80 columns.
    (propertize "%3c" 'face
                (if (>= (current-column) 80)
                    'mode-line-80col
                  'mode-line-position)))

   " "

   ;; Evil state
   (:eval
    (if (and (featurep 'evil) (true? evil-mode))
        evil-mode-line-tag
      ""))

   ;; --------------------------------------------------------------------------
   ;; Mail status.
   (:eval
    (if (true? cbm:mode-line-indicator)
        (concat " " cbm:mode-line-indicator " ")
      ""))

   " "

   ;; Show clock for window 1.
   (:eval (if (equal 1 (length (window-list)))
              (propertize (format-time-string "[%h %d, %H:%M] ")
                          'face 'mode-line-minor-mode)
            ""))

   ;; Pomodoro
   ;;
   (:eval
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
                  br))))

     ;; Show just the time when on a break.
     ((and (true? org-pomodoro-mode-line)
           (concat org-pomodoro-mode-line)))
     (t
      "")))

   ;; Current org notes file
   (:eval
    (if (and (equal 1 (length (window-list)))
             (true? org-init-notes-file)
             (not (equal org-init-notes-file org-default-notes-file)))
        (propertize
         (concat " [" (f-filename (f-no-ext org-default-notes-file)) "] ")
         'face 'modeline-org-notes-file-indicator)
      ""))

   ;; --------------------------------------------------------------------------
   ;; File status.
   (:eval
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
   " "
   ;; --------------------------------------------------------------------------
   ;; Buffer name and path.
   (:eval (cbmd:description))

   ;; --------------------------------------------------------------------------
   ;; Narrowing
   " %n "

   ;; --------------------------------------------------------------------------
   ;; Mode details.

   ;; Major mode.
   " %[" (:propertize mode-name face mode-line-mode) "%] "

   ;; Minor modes.
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode))
   (:propertize mode-line-process
                face mode-line-process)
   " "
   (:eval (or (ignore-errors
                (propertize modeline-mail-indicator 'face 'mode-line-emphasis))
              ""))
   " "
   (global-mode-string global-mode-string)))

(defvar cb:modeline-timer
  (run-with-idle-timer 30 t 'force-mode-line-update t)
  "Timer that updates the modeline once a minute.")

(provide 'config-modeline)

;;; config-modeline.el ends here
