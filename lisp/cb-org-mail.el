;;; cb-org-mail.el --- Configuration for email

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130902.0348

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

;; Configuration for email. Adapts org-mode for sending emails.

;;; Code:

(require 'message)
(require 'org)
(require 'org-mime)
(require 'bbdb)
(require 's)
(require 'dash)
(autoload 'goto-address-find-address-at-point "goto-addr.el")

;; Define a command for sending HTML emails.  Uses ido to read email addresses
;; and org-mode for message composition.
(defun org-compose-mail (region to subject)
  "Start composing a new message.

- REGION is inserted into the compose buffer, if a region was active.

- TO is either the email address at point or an address read by from the user.

- SUBJECT is a string read from the user."
  (interactive
   (list
    ;; Extract the current region as a quote.
    (when (region-active-p)
      (buffer-substring (region-beginning) (region-end)))
    ;; Get email address at point or read from user.
    (or (goto-address-find-address-at-point)
        (cbom:read-email))
    ;; Read the message subject interactively.
    (read-string "Subject: ")))

  (let ((compose-buf (generate-new-buffer "*new message*")))
    ;; Split the window, restoring the previous window state after sending
    ;; the message.
    (with-window-restore
      (select-window (display-buffer-at-bottom compose-buf nil))
      ;; Configure message compose buffer.  Restore window state when killing
      ;; the compose buffer.
      (org-mode)
      (hook-fn 'kill-buffer-hook :local t (restore))
      (buffer-local-set-key (kbd "<tab>") 'cbom:message-tab)
      (buffer-local-set-key (kbd "C-c q") 'kill-this-buffer)
      (buffer-local-set-key (kbd "C-c d") 'cbom:message-send)
      (buffer-local-set-key (kbd "C-c C-a") 'mail-add-attachment-ido)
      ;; Set org buffer properties.
      (insert (concat "#+TO: " to "\n"
                      "#+SUBJECT: " subject "\n"
                      org-mime-default-header))
      ;; Position in body.
      (cb:append-buffer)
      ;; Insert current region as quote.
      (when region
        (save-excursion
          (newline)
          (insert region)))
      (message "<C-c d> to send message, <C-c q> to cancel."))))

(defun cbom:promote-heading-top-level ()
  "Promote the current org-mode subtree to the top level."
  (when (org-at-heading-p)
    (while (ignore-errors (org-promote-subtree) t))))

(defun org-compose-mail-subtree (to subject)
  "Compose a new message based on the current org subtree.

- TO is either the email address at point or an address read by from the user.

- SUBJECT is a string read from the user."
  (interactive
   (list
    ;; Get email address at point or read from user.
    (or (goto-address-find-address-at-point)
        (cbom:read-email))
    ;; Read the message subject interactively.
    (read-string "Subject: " (substring-no-properties (org-get-heading t t)))))
  (save-restriction
    (org-narrow-to-subtree)
    (let* ((s (s-trim (buffer-substring (point-min) (point-max))))
           (s (with-temp-buffer
                (org-mode)
                (insert s)
                (goto-char (point-min))
                (cbom:promote-heading-top-level)
                (buffer-string))))
      (org-compose-mail s to subject))))

(defun cbom:read-email ()
  "Read an email address from BBDB using ido."
  (require 'bbdb)
  (ido-completing-read
   "New email to: "
   (->> (bbdb-records)
     (-filter 'bbdb-record-mail)
     (-map (lambda (record)
             (--map (format "%s <%s>" (bbdb-record-name record) it)
                    (bbdb-record-mail record))))
     (-flatten))))

(defun cbom:message-send ()
  "Export the org message compose buffer to HTML and send as an email.
Kill the buffer when finished."
  ;; Create a new message, extracting header values from the compose buffer's
  ;; headers.
  (interactive)
  (let* ((str (buffer-string))
         (headers (cbom:header->alist str)))
    (save-window-excursion
      (compose-mail (cdr (assoc "TO" headers))
                    (cdr (assoc "SUBJECT" headers))
                    (->> (list (assoc "CC" headers)
                               (assoc "BCC" headers)
                               (assoc "FCC" headers)
                               (assoc "GCC" headers))
                      (-remove 'null)))
      ;; Prepare message body.
      (message-goto-body)
      (insert str)
      (org-mime-htmlize nil)
      (message-send-and-exit)))
  ;; Restore previous window state.
  (kill-this-buffer))

(defun cbom:message-tab ()
  "Complete email addresses in the header, otherwise cycle headlines."
  (interactive)
  (if (s-starts-with? "#+" (current-line))

      (let ((beg (line-beginning-position)))
        (save-restriction
          (narrow-to-region beg (line-end-position))
          (bbdb-complete-mail)
          ;; `bbdb-complete-mail' puts
          ;; addresses on new lines. Rejoin
          ;; the lines.
          (save-excursion
            (goto-char beg)
            (while (search-forward-regexp
                    (rx (* space) "," (* (any space "\n")))
                    nil t)
              (replace-match ", ")))))
    (call-interactively 'org-cycle)))

(defun cbom:header->alist (str)
  "Extract the header values from the contents of the given buffer string STR."
  (->> (s-lines str)
    ;; Get header lines
    (--take-while (or (s-matches? (rx bol "#+" (* nonl) ":") it)
                      (s-blank? it)))
    (-remove 's-blank?)
    ;; Create alist of keywords -> values
    (--map (cl-destructuring-bind (_ key val &rest rest)
               (s-match (rx bol "#+" (group (+ nonl))
                            (not (any "\\")) ":" (* space)
                            (group (+ nonl)))
                        it)
             (cons (s-upcase key) val)))))

(provide 'cb-org-mail)

;;; cb-org-mail.el ends here
