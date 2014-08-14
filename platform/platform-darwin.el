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

(autoload 'thing-at-point-url-at-point "thingatpt")

(-when-let (gls (executable-find "gls"))
  (setq ls-lisp-use-insert-directory-program t
        insert-directory-program gls))

(when (s-ends-with? "fish" (getenv "SHELL"))
  (custom-set-variables
   '(shell-file-name "/bin/bash")
   '(explicit-shell-file-name shell-file-name))

  (setenv "SHELL" shell-file-name))

(when (equal system-type 'darwin)
  (custom-set-variables
   '(trash-directory "~/.Trash/"))

  (cb:install-package 'exec-path-from-shell t)
  (exec-path-from-shell-initialize)

  (bind-key* "s-p" 'ps-print-with-faces-dwim)

  (after 'bbdb
    (require 'osx-bbdb))

  (let ((terminfo (expand-file-name "~/.terminfo")))
    (unless (file-exists-p terminfo)
      (start-process
       "tic" " tic" "tic"
       "-o" terminfo
       "/Applications/Emacs.app/Contents/Resources/etc/e/eterm-color.ti")))

  (after 'evil
    (evil-global-set-key 'normal (kbd "g o") 'mac-open-dwim)
    (evil-global-set-key 'normal (kbd "g O") 'mac-reveal-in-finder))

  (custom-set-variables
   '(shell-command-switch "-lc")
   '(insert-directory-program (or (executable-find "gls") "ls"))
   '(starttls-gnutls-program (executable-find "gnutls-cli"))
   '(starttls-use-gnutls t)))

(when (and (equal system-type 'darwin) (not window-system))
  (setq interprogram-cut-function   'cb:osx-copy
        interprogram-paste-function 'cb:osx-paste))

(defun ps-print-with-faces-dwim ()
  "Perform a context-sensitive printing command."
  (interactive)
  (call-interactively
   (if (region-active-p)
       'ps-print-region-with-faces
     'ps-print-buffer-with-faces)))

(defun osx-find-system-sound (name)
  "Find a system alert matching NAME."
  (when (equal system-type 'darwin)
    (-first (~ s-matches? name) (f-files "/System/Library/Sounds"))))

(defun osx-play-system-sound (name)
  "Play alert matching NAME."
  (when (equal system-type 'darwin)
    (-when-let (snd (osx-find-system-sound name))
      (start-process "appt alert" " appt alert" "afplay" snd))))

(defun cb:osx-paste ()
  (shell-command-to-string "pbpaste"))

(defun cb:osx-copy (text &optional _push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

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

(provide 'platform-darwin)

;;; platform-darwin.el ends here
