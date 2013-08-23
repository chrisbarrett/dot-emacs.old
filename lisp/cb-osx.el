;;; cb-osx.el --- Configuration for OS X

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0024

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

;; Configuration for OS X

;;; Code:

(require 'use-package)
(require 'cb-lib)
(autoload 'thing-at-point-url-at-point "thingatpt")

(use-package exec-path-from-shell
  :ensure t
  :defer  t
  :if     (or (daemonp) (window-system))
  :init   (hook-fn 'after-init-hook (require 'exec-path-from-shell))
  :config
  (progn
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PKG_CONFIG_PATH")
    (exec-path-from-shell-copy-env "CFLAGS")))

;; Enable mouse support in terminal.
(use-package mouse
  :if (not (display-graphic-p))
  :defines (mouse-sel-mode)
  :config
  (progn
    (xterm-mouse-mode t)
    (defun track-mouse (_))
    (global-set-key [mouse-4] (command (scroll-down 1)))
    (global-set-key [mouse-5] (command (scroll-up 1)))))

(use-package osx-bbdb
  :if (not noninteractive))

;; Set terminfo so ansi-term displays shells correctly.

(let ((terminfo (expand-file-name "~/.terminfo")))
  (unless (file-exists-p terminfo)
    (start-process
     "tic" " tic" "tic"
     "-o" terminfo
     "/Applications/Emacs.app/Contents/Resources/etc/e/eterm-color.ti")))

;; Play Mail's message sent sound when sending mail.

(hook-fn 'async-smtpmail-sent-hook
  (let ((snd "/Applications/Mail.app/Contents/Resources/Mail Sent.aiff"))
    (when (file-exists-p snd)
      (start-process "Mail sent" " mail sent" "afplay" snd))))

;; Use system clipboard.

(unless window-system

  (defun cb:osx-paste ()
    (shell-command-to-string "pbpaste"))

  (defun cb:osx-copy (text &optional _push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function   'cb:osx-copy
        interprogram-paste-function 'cb:osx-paste))

(use-package org-mac-iCal
  :ensure t
  :commands (org-mac-iCal)
  :init
  (after 'org-agenda
    ;; Package.el doesn't initialize this package for some reason.
    ;; Add it to the load path manually.
    (->> (f-entries cb:elpa-dir)
      (--first (s-contains? "org-mac-iCal" it))
      (add-to-list 'load-path))

    (add-to-list 'org-agenda-custom-commands
                 '("I" "Import diary from iCal" agenda ""
                   ((org-agenda-mode-hook
                     (lambda () (org-mac-iCal)))))
                 'append)

    (hook-fn 'org-agenda-cleanup-fancy-diary-hook
      "Ensure all-day events are not orphaned below TODO items."
      (goto-char (point-min))
      (save-excursion
        (while (re-search-forward "^[a-z]" nil t)
          (goto-char (match-beginning 0))
          (insert "0:00-24:00 ")))
      (while (re-search-forward "^ [a-z]" nil t)
        (goto-char (match-beginning 0))
        (save-excursion
          (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
        (insert (match-string 0))))))

(defun visual-url-at-point ()
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
   (list (-if-let (url (or (visual-url-at-point)
                           (when (boundp 'w3m-current-url) w3m-current-url)
                           (when (derived-mode-p 'dired-mode) (dired-get-file-for-visit))
                           (buffer-file-name)))
           (read-string (format "Open (%s): " url) nil t url)
           (read-string "Open: " nil t))))
  (shell-command (format "open '%s'" open-arg)))

(bind-key* "S-s-<return>" 'mac-open-dwim)

;; Use gnutls when sending emails.
(when (equal system-type 'darwin)
    (setq starttls-gnutls-program (executable-find "gnutls-cli")
          starttls-use-gnutls t))

(provide 'cb-osx)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-osx.el ends here
