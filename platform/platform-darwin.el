;;; platform-darwin.el --- Configuration for OS X hosts

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

;; Configuration for OS X hosts

;;; Code:

(require 'utils-common)
(require 'utils-shell)
(require 'osx-bbdb)

(autoload 'thing-at-point-url-at-point "thingatpt")

(custom-set-variables
 '(trash-directory "~/.Trash/")
 '(shell-command-switch "-lc")
 '(insert-directory-program (or (executable-find "gls") "ls"))
 '(starttls-gnutls-program (executable-find "gnutls-cli"))
 '(starttls-use-gnutls t)
 '(interprogram-cut-function 'cb:osx-copy)
 '(interprogram-paste-function 'cb:osx-paste))

;;; Use bash for shell command execution if Fish is the default.

(when (s-ends-with? "fish" (getenv "SHELL"))
  (custom-set-variables
   '(shell-file-name "/bin/bash")
   '(explicit-shell-file-name shell-file-name))

  (setenv "SHELL" shell-file-name))

;;; Copy $PATH from shell.

(cb:install-package 'exec-path-from-shell t)
(exec-path-from-shell-initialize)

;;; Print with faces using cmd+P

(defun ps-print-with-faces-dwim ()
  "Perform a context-sensitive printing command."
  (interactive)
  (call-interactively
   (if (region-active-p)
       'ps-print-region-with-faces
     'ps-print-buffer-with-faces)))

(bind-key* "s-p" 'ps-print-with-faces-dwim)

;;; Use GNU ls where available.

(-when-let (gls (executable-find "gls"))
  (setq ls-lisp-use-insert-directory-program t)
  (setq insert-directory-program gls))

;;; Utilities

(defun osx-find-system-sound (name)
  "Find a system alert matching NAME."
  (when (equal system-type 'darwin)
    (-first (~ s-matches? name) (f-files "/System/Library/Sounds"))))

(defun osx-play-system-sound (name)
  "Play alert matching NAME."
  (when (equal system-type 'darwin)
    (-when-let (snd (osx-find-system-sound name))
      (start-process "appt alert" " appt alert" "afplay" snd))))

;;; Copy/paste integration

(defun cb:osx-paste ()
  (shell-command-to-string "pbpaste"))

(defun cb:osx-copy (text &optional _push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;;; Open with default apps or in Finder.

(defun mac-reveal-in-finder ()
  "Open the current directory in the Finder."
  (interactive)
  (%-sh "open ."))

(defun cb:visual-url-at-point ()
  "Find a URL at point."
  (or
   ;; Find urls at point.
   (thing-at-point-url-at-point)
   (get-text-property (point) 'shr-url)
   ;; Extract org-mode links.
   (when (and (fboundp 'org-in-regexp)
              (boundp 'org-bracket-link-regexp)
              (org-in-regexp org-bracket-link-regexp 1))
     (org-link-unescape (org-match-string-no-properties 1)))))

(defun mac-open-dwim (open-arg)
  "Pass OPEN-ARG to OS X's open command.
When used interactively, makes a guess at what to pass."
  (interactive
   (list
    (ido-read-file-name
     "Open: " nil (or
                   (cb:visual-url-at-point)
                   (and (boundp 'w3m-current-url) w3m-current-url)
                   (and (derived-mode-p 'dired-mode)
                        (read-file-name(dired-get-file-for-visit)))
                   (buffer-file-name)))))

  (%-sh (format "open '%s'" open-arg)))

(after 'evil
  (evil-global-set-key 'normal (kbd "g o") 'mac-open-dwim)
  (evil-global-set-key 'normal (kbd "g O") 'mac-reveal-in-finder))

;;; Growl

(defvar cb:growl-default-icon
  (-first 'f-exists?
          '("/Applications/Emacs.app/Contents/Resources/Emacs.icns"
            "~/Applications/Emacs.app/Contents/Resources/Emacs.icns")))

(cl-defun growl (title
                 message
                 &optional (icon cb:growl-default-icon))
  "Display a growl notification.
Fall back to `message' if growlnotify is not installed.
The notification will have the given TITLE and MESSAGE."
  (let ((growl-program "growlnotify"))
    (if (executable-find growl-program)
        ;; Call growl
        (let ((proc (start-process "growl" nil
                                   growl-program
                                   title
                                   "-n" "Emacs"
                                   "-a" "Emacs"
                                   "--image" icon)))
          (process-send-string proc message)
          (process-send-string proc "\n")
          (process-send-eof proc))
      ;; Fall back to message.
      (message "%s. %s" title message))))

;;; Org

(hook-fn 'org-timer-start-hook (growl "Timer Started" "" org-unicorn-png))
(hook-fn 'org-timer-done-hook  (growl "Timer Finished" "" org-unicorn-png))
(hook-fn 'org-timer-done-hook  (osx-play-system-sound "glass"))

;;; Appt

(defadvice appt-display-message (around growl-with-sound activate)
  "Play a sound and display a growl notification for appt alerts."
  ;; Show notification.
  (let ((title (-listify (ad-get-arg 0)))
        (mins (-listify (ad-get-arg 1))))
    (--each (-zip-with 'list title mins)
      (growl (cond ((zerop mins) "Appointment (now)")
                   ((= 1 mins)   "Appointment (1 min)")
                   (t (format "Appointment (%s mins)" mins)))
             (cl-destructuring-bind (whole time desc)
                 (s-match (rx bol
                              (group (+ digit) ":" (+ digit))
                              (* space)
                              (group (* nonl)))
                          title)
               desc))))
  ;; Play sound.
  (osx-play-system-sound "blow"))

(provide 'platform-darwin)

;;; platform-darwin.el ends here
