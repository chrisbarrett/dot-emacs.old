;;; config-eshell.el --- Configuration for eshell

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

;; Configuration for eshell

;;; Code:

(require 'utils-common)
(require 'utils-shell)
(require 'utils-buffers)
(require 'config-theme)

(setenv "EDITOR" "emacsclient")

(setenv "TERM" "ansi")

(custom-set-variables
 '(eshell-cmpl-ignore-case t)
 '(eshell-highlight-prompt nil)
 '(eshell-prompt-regexp (rx bol (* space) (or "#" ":") space))
 '(eshell-prompt-function 'cb-eshell:format-prompt))

(defun cb:term-cycle (&optional arg)
  "Cycle through various terminal window states."
  (interactive "P")
  (cond
   (arg
    (eshell arg))

   ((--none? (with-current-buffer it (derived-mode-p 'eshell-mode))
             (buffer-list))
    (eshell))

   ;; If terminal is maximized, restore previous window config.
   ((and (derived-mode-p 'eshell-mode)
         (equal 1 (length (window-list))))
    (or (ignore-errors (jump-to-register :term-fullscreen) t)
        (bury-buffer)))

   ;; If we're looking at the terminal, maximise it.
   ((derived-mode-p 'eshell-mode)
    (delete-other-windows))

   ;; Otherwise show the terminal.
   (t
    ;; Hide the term window if it's visible.
    (-when-let (win (--first
                     (with-current-buffer (window-buffer it)
                       (derived-mode-p 'eshell-mode))
                     (window-list)))
      (delete-window win))
    ;; Save this configuration to a register so that it can be restored
    ;; for later positions in the cycle.
    (window-configuration-to-register :term-fullscreen)
    ;; Show terminal.
    (switch-to-buffer (--first-buffer (derived-mode-p 'eshell-mode))))))

(bind-key* "<f1>" 'cb:term-cycle)

(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min)
                   (save-excursion
                     (goto-char (point-max))
                     (search-backward-regexp eshell-prompt-regexp)
                     (let ((prompt-line (line-beginning-position)))
                       (forward-line -1)
                       (if (s-matches? (rx bol (+ space "[")) (current-line))
                           (line-beginning-position)
                         prompt-line))))))

(hook-fn 'eshell-mode-hook
  (define-key eshell-mode-map (kbd "C-l") 'eshell/clear))

(defface eshell-prompt-sep
  '((t :inherit 'font-lock-comment-face))
  "Face for separators in the eshell prompt."
  :group 'cb-eshell)

(defun cb-eshell:current-dir ()
  (let* ((cwd (f-short (eshell/pwd)))
         (colour (if (s-starts-with? "/" cwd)
                     solarized-hl-orange
                   solarized-hl-cyan))
         (dir (propertize cwd 'face (list :foreground colour))))

    (list
     (cons "cwd" dir))))

(defun cb-eshell:directory-info ()
  (let* ((cwd (if (s-blank? (eshell/pwd)) "." (eshell/pwd)))
         (ls-result (%-string "ls -ld" (%-quote cwd))))
    (unless (s-matches? "No such file or directory" ls-result)
      (cl-destructuring-bind (&optional perms _links owner group &rest _)
          (s-split (rx (+ space)) ls-result)
        (unless (equal owner (%-string "whoami"))
          (list
           (cons "mode"
                 (propertize perms 'face `(:foreground ,solarized-hl-cyan)))
           (cons "owner"
                 (propertize owner 'face `(:foreground ,solarized-hl-cyan)))
           (cons "group"
                 (propertize group 'face `(:foreground ,solarized-hl-cyan)))))))))

(defun cb-eshell:git-anything-untracked? ()
  "Non-nil if there are any untracked files in the current git repo."
  (->> (%-string "git status -u -s --porcelain")
    (s-split "\n")
    (--any? (s-matches? (rx (* space) "??") it))))

(defun cb-eshell:git-status ()
  (require 'magit)
  (when (and (executable-find "git")
             (locate-dominating-file (eshell/pwd) ".git"))
    (let ((branch (%-string "git rev-parse --abbrev-ref HEAD"))
          (sha (substring (%-string "git rev-parse HEAD") 0 7))
          (state (-non-null
                  (list
                   (when (cb-eshell:git-anything-untracked?)
                     (propertize "new" 'face
                                 `(:foreground ,solarized-hl-cyan)))
                   (when (magit-anything-unstaged-p)
                     (propertize "mod" 'face
                                 `(:foreground ,solarized-hl-orange)))
                   (when (magit-anything-staged-p)
                     (propertize "staged" 'face
                                 `(:foreground ,solarized-hl-green)))))))
      (-non-null
       (list

        (cons "branch"
              (propertize branch 'face
                          `(:foreground ,solarized-hl-yellow)))
        (cons "rev" sha)

        (when state
          (cons "state"
                (s-join (propertize "," 'face 'eshell-prompt-sep)
                        state))))))))

(defun cb-eshell:prompt-symbol ()
  (let ((ch (if (= (user-uid) 0) "#" ":"))
        (colour (if (zerop eshell-last-command-status)
                    solarized-hl-cyan
                  solarized-hl-red)))
    (propertize ch 'face (list :foreground colour))))

(defun cb-eshell:make-header-list ()
  "Create a list of alists, representing lines in the header.
Each alist has a string as the key and a "
  (list
   (cb-eshell:current-dir)
   (cb-eshell:directory-info)
   (cb-eshell:git-status)))

(cl-defun cb-eshell:format-cons ((k . v))
  (if (equal k "cwd")
      v
    (concat (propertize (concat k "=") 'face 'eshell-prompt-sep)
            v)))

(defun cb-eshell:header-list->string (header-list)
  (concat
   (propertize " [ " 'face 'eshell-prompt-sep)

   (->> header-list
     (--map (->> it (-map 'cb-eshell:format-cons) (s-join " ")))
     (-remove (C s-blank? s-trim))
     (s-join (propertize " | " 'face 'eshell-prompt-sep)))

   (propertize " ]" 'face 'eshell-prompt-sep)
   "\n"))

(defvar-local cb-eshell:last-header nil)

(defun cb-eshell:format-prompt ()
  "Format the prompt to display in eshell."
  (let* ((hlist (cb-eshell:make-header-list))
         (diff (-difference hlist cb-eshell:last-header)))
    (setq cb-eshell:last-header hlist)
    (concat
     (when cb-eshell:last-header "\n")
     (when diff (cb-eshell:header-list->string diff))
     (propertize (concat " " (cb-eshell:prompt-symbol) " ")
                 'read-only t
                 'front-sticky 'read-only
                 'rear-nonsticky 'read-only))))

(defun cb-eshell:find-file-or-files (files)
  "Open FILES.
If files is a small list, open each file and tile windows.
Otherwise call `find-file' on FILES."
  (let* ((fs (-flatten files))
         (len (length fs)))
    (cond ((equal 1 len)
           (find-file (car fs)))
          ((y-or-n-p (format "Open %s files? " len))
           (if (<= len 5)
               (let ((bs (-map 'find-file-noselect fs)))
                 (message "BS: %s" bs)
                 (expose-buffers bs))
             (-map 'find-file fs))
           (message "Viewing %s" (s-join ", " fs)))

          (t
           (user-error "Aborted")))))

(after 'smartparens
  (add-hook 'eshell-mode-hook 'smartparens-mode))

(defun cb-eshell:format-buffer-name ()
  "Make a buffer name based on the current directory."
  (generate-new-buffer-name
   (concat "eshell: " (f-short (eshell/pwd)))))

(hook-fn 'eshell-directory-change-hook
  (rename-buffer (cb-eshell:format-buffer-name)))


(provide 'config-eshell)

;;; config-eshell.el ends here
