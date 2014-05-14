;;; config-mail.el --- Email configuration

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

;; Email configuration

;;; Code:

(require 'utils-common)
(require 'utils-ui)

(setq message-kill-buffer-on-exit t)

(defvar async-smtpmail-sent-hook nil)

(defun async-smtpmail-send-it (&rest _)
  "Send mail asynchronously using another Emacs process."
  (let ((to (message-field-value "To")))
    (message "Delivering message to %s..." to)
    (async-start
     `(lambda ()
        (require 'smtpmail)
        (with-temp-buffer
          (insert ,(buffer-string))
          ;; Pass in the variable environment for smtpmail.
          (setq user-mail-address ,user-mail-address
                smtpmail-mail-address ,smtpmail-mail-address
                smtpmail-smtp-server ,smtpmail-smtp-server
                smtpmail-smtp-service ,smtpmail-smtp-service
                message-directory ,message-directory
                message-auto-save-directory ,message-auto-save-directory)

          (smtpmail-send-it)
          nil))
     `(lambda (&rest _)
        (message "Delivering message to %s...done" ,to)
        (run-hooks 'async-smtpmail-sent-hook)))))

(setq message-send-mail-function 'async-smtpmail-send-it
      send-mail-function 'async-smtpmail-send-it)

(when (equal system-type 'darwin)
  (hook-fn 'async-smtpmail-sent-hook
    (let ((snd "/Applications/Mail.app/Contents/Resources/Mail Sent.aiff"))
      (when (file-exists-p snd)
        (start-process "Mail sent" " mail sent" "afplay" snd)))))

(defun mail-add-attachment-ido (file)
  (interactive (list (ido-read-file-name "Attach file: " )))
  (mail-add-attachment file))

(after 'message
  (define-key message-mode-map (kbd "C-c C-a") 'mail-add-attachment-ido))

(cb:install-package 'notmuch)

(require 'notmuch-maildir-fcc)

(after '(notmuch evil)
  (define-keys notmuch-search-mode-map
    "j" 'next-line
    "k" 'previous-line
    "C-f" 'evil-scroll-page-down
    "C-b" 'evil-scroll-page-up)

  (define-keys notmuch-show-mode-map
    "C-f" 'evil-scroll-page-down
    "C-b" 'evil-scroll-page-up)
  (evil-add-hjkl-bindings notmuch-show-mode-map))

(provide 'config-mail)

;;; config-mail.el ends here
