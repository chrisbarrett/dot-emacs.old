;;; cb-modeline.el --- Configuration for modeline

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0032

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

;; Configuration for modeline

;;; Code:

;; Lighten background for default theme.
(unless (or (daemonp) (display-graphic-p))
  (set-face-attribute 'mode-line nil :background "gray85"))

(defun* cb:vc-state->letter (&optional (file (buffer-file-name)))
  "Return a single letter to represent the current version-control status."
  (case (ignore-errors (vc-state file))
    ((up-to-date)           " ")
    ((edited)               (propertize "M" 'face '(:foreground "red")))
    ((needs-merge conflict) (propertize "!" 'face '(:foreground "red")))
    ((added)                (propertize "A" 'face '(:foreground "green")))
    ((removed)              (propertize "D" 'face '(:foreground "red")))
    ((ignored)              (propertize "-" 'face 'modeline-vc-unknown-face))
    (t                      (propertize "?" 'face 'modeline-vc-unknown-face))))

(defun* cb:vc-file-uptodate? (&optional (file (buffer-file-name)))
  "Non-nil if FILE is up-to-date."
  (ignore-errors (equal 'up-to-date (vc-state file))))

(defun* cb:shorten-directory (dir &optional (max-length 30))
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

(defun* cb:propertize-file-directory
    (&optional (filepath (file-name-directory (buffer-file-name))))
  "Separate tramp info from the given filepath."
  (flet ((face
          (str face)
          (propertize str 'face face)))

    (destructuring-bind (&optional method user host file &rest _)
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
  '((t (
        :foreground "VioletRed3"
                    :inherit 'mode-line)))
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
  '((t (
        :foreground "#4271ae"
                    :box '(:line-width 2 :color "#4271ae")
                    :inherit 'mode-line)))
  "Face for readonly indicator."
  :group 'modeline)

(defface mode-line-modified
  '((t (
        :foreground "#c82829"
                    :inherit 'mode-line)))
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
  '((((background dark))
     (:foreground "#eab700" :weight bold))
    (((background light))
     (:foreground "gray40" :weight bold))
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
  '((t (
        :foreground "#718c00"
                    :inherit 'mode-line)))
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

(autoload 'vc-git-root "vc-git")

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
   (:eval (if (buffer-file-name) (cb:propertize-file-directory) ""))
   (:propertize "%b" face mode-line-filename)

   ;; --------------------------------------------------------------------------
   ;; Narrowing
   " %n "

   ;; --------------------------------------------------------------------------
   ;; Mode details.

   ;; Major mode.
   " %["
   (:propertize mode-name
                face mode-line-mode)
   "%] "

   ;; ERT status.
   (:eval (when (cb:truthy? 'ert-modeline-mode)
            (set-face-bold 'ertml-failing-face t)
            (let ((s (s-trim ertml--status-text)))
              (if (s-matches? (rx digit) s)
                  (propertize s 'face 'ertml-failing-face)
                (propertize s 'face 'bold)))))

   ;; Minor modes.
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode))
   (:propertize mode-line-process
                face mode-line-process)
   (global-mode-string global-mode-string)))


(provide 'cb-modeline)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-modeline.el ends here
