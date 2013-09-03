;;; cb-mail.el --- Configuration for emaiL

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

(require 'use-package)
(require 'cb-lib)
(autoload 'bbdb-record-name "bbdb")
(autoload 'bbdb-message-clean-name-default "bbdb-mua")
(autoload 'bbdb-record-mail "bbdb")
(autoload 'goto-address-find-address-at-point "goto-addr.el")
(autoload 'bbdb-records "bbdb")
(autoload 'bbdb-complete-mail "bbdb-com")

;; Use org-mode-style tables and structure editing in message-mode.
(after 'message
  (add-hook 'message-mode-hook 'orgstruct++-mode)
  (add-hook 'message-mode-hook 'orgtbl-mode)
  (define-key message-mode-map (kbd "C-c RET RET") 'org-ctrl-c-ret))

;; Define a command for sending HTML emails.  Uses ido to read email addresses
;; and org-mode for message composition.
(defun cb-org:compose-mail (region to subject)
  "Start composing a new message.
* REGION is inserted into the compose buffer, if a region was active.
* TO is either the email address at point or an address read by from the user.
* SUBJECT is a string read from the user."
  (interactive
   (list
    ;; Extract the current region as a quote.
    (when (region-active-p)
      (cb-org:buffer-substring-to-quote (region-beginning) (region-end)))
    ;; Get email address at point or read from user.
    (or (goto-address-find-address-at-point)
        (cb-org:read-email))
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
      (buffer-local-set-key (kbd "<tab>") 'cb-org:message-tab)
      (buffer-local-set-key (kbd "C-c q") 'kill-this-buffer)
      (buffer-local-set-key (kbd "C-c c") 'cb-org:message-send)
      (buffer-local-set-key (kbd "C-c C-a") 'mail-add-attachment-ido)
      ;; Set org buffer properties.
      (insert (format (s-unlines
                       "#+TO: %s"
                       "#+SUBJECT: %s"
                       "#+OPTIONS: toc:nil num:nil"
                       "\n")
                      to subject))
      ;; Position in body.
      (cb:append-buffer)
      ;; Insert current region as quote.
      (when region
        (save-excursion
          (newline)
          (insert region)))
      (message "<C-c c> to send message, <C-c q> to cancel."))))

(defun cb-org:read-email ()
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

(defun cb-org:buffer-substring-to-quote (beg end)
  "Format the portion of the current buffer from BEG to END as a quote or code block."
  (let ((str (s-trim (buffer-substring-no-properties beg end))))
    ;; If the captured text is source code, wrap it in a code block. Otherwise
    ;; wrap it in a block quote.
    (if (derived-mode-p 'prog-mode)
        (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n"
                ;; Determine name of mode to use.
                (case major-mode
                  (c-mode 'C)
                  (emacs-lisp-mode 'elisp)
                  (otherwise
                   (car (s-split-words (symbol-name major-mode)))))
                str)
      (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" str))))

(defun cb-org:message-send ()
  "Export the org message compose buffer to HTML and send as an email.
Kill the buffer when finished."
  ;; Create a new message, extracting header values from the compose buffer's
  ;; headers.
  (interactive)
  (let* ((str (buffer-string))
         (headers (cb-org:header->alist str)))
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

(defun cb-org:message-tab ()
  "Complete email addresses in the header, otherwise cycle headlines."
  (interactive)
  (if (s-starts-with? "#+"(current-line))

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

(defun* cb-org:header->alist (str)
  "Extract the header values from the contents of the given buffer string STR."
  (->> (s-lines str)
    ;; Get header lines
    (--take-while (or (s-matches? (rx bol "#+" (* nonl) ":") it)
                      (s-blank? it)))
    (-remove 's-blank?)
    ;; Create alist of keywords -> values
    (--map (destructuring-bind (_ key val &rest rest)
               (s-match (rx bol "#+" (group (+ nonl))
                            ":" (* space)
                            (group (+ nonl)))
                        it)
             (cons (s-upcase key) val)))))

(bind-key* "C-x m" 'cb-org:compose-mail)
(bind-key* "C-x M" 'compose-mail)

(use-package muttrc-mode
  :ensure t
  :mode ("muttrc$" . muttrc-mode))

;; -----------------------------------------------------------------------------
;; Define custom mode for mutt message composition.

(defvar org-mutt-compose-mode-map
  (let ((km (make-sparse-keymap)))
    (define-keys km
      "C-c c" 'org-mutt-compose-finished
      "C-c q" 'org-mutt-compose-cancel)
    km))

(define-minor-mode org-mutt-compose-mode
  "Configures an org buffer for editing messages for Mutt."
  nil " OrgMutt" org-mutt-compose-mode-map)

(defun org-mutt:message-buffer? ()
  (and (-contains? (s-split "/" (buffer-file-name)) ".mutt")
       (s-starts-with? "mutt" (f-filename (buffer-file-name)))))

;;;; Interactive commands

(defun org-mutt-compose-cancel ()
  "Cancel editing the message."
  (interactive)
  (revert-buffer t t)
  (server-done))

(defun org-mutt-compose-finished ()
  "Convert the buffer to HTML and finish."
  (interactive)
  ;; Replace buffer with multipart representation.
  (let ((bod (buffer-string)))
    (delete-region (point-min) (point-max))
    (insert (org-mutt:format-message
             org-mutt:original-header-string
             bod)))
  ;; Clean up buffer text.
  (let ((delete-trailing-lines t))
    (delete-trailing-whitespace))
  ;; Finish editing with the server.
  (save-buffer)
  (server-done))

;;;; Message initialisation

(defvar org-mutt:org-header "#+OPTIONS: toc:nil num:nil"
  "The default org header to insert into new messages.")

(defun org-mutt:header-end-pos ()
  (save-excursion
    (goto-char (point-min))
    (while (not (s-blank? (current-line)))
      (forward-line))
    (point)))

(defun org-mutt:header-string ()
  (s-trim (buffer-substring-no-properties
           (point-min) (org-mutt:header-end-pos))))

(defun org-mutt:message-body ()
  (s-trim (buffer-substring-no-properties
           (org-mutt:header-end-pos) (point-max))))

(defvar-local org-mutt:original-header-string nil
  "The original message headers set by mutt.")

(defun org-mutt:prepare-message-buffer ()
  "Prepare a new message by formatting the buffer and setting modes."
  ;; Store the components of the message from mutt.
  (setq org-mutt:original-header-string (org-mutt:header-string))
  (let ((bod (org-mutt:message-body)))
    ;; Reinitialise for org.
    (atomic-change-group
      (delete-region (point-min) (point-max))
      (insert org-mutt:org-header)
      (newline)
      (insert bod))))

(defun org-mutt:maybe-edit ()
  "If this is a mutt message, prepare the buffer for editing."
  (when (org-mutt:message-buffer?)
    (org-mode)
    (org-mutt-compose-mode +1)
    (org-mutt:prepare-message-buffer)
    ;; Enter insertion state
    (when (fboundp 'evil-insert)
      (evil-insert 1))
    ;; Display info on key bindings.
    (run-with-timer
     0.15 nil
     (lambda () (message "<C-c c> to send message, <C-c q> to cancel.")))))

;; Prepare any messages sent for editing by mutt.
(add-hook 'server-visit-hook 'org-mutt:maybe-edit)

;;; Multipart message formatting.
;;;
;;; See http://www.w3.org/Protocols/rfc1341/7_2_Multipart.html

(defun org-mutt:delimiter (boundary &optional mime-type)
  (concat "\r\n--" boundary
          (if mime-type
            (concat "\r\n" "Content-Type: " mime-type)
            "\r\n")))

(defun org-mutt:close-delimiter (boundary)
  (concat "\r\n--" boundary "--"))

(defun org-mutt:encapsulation (boundary mime-type body-part)
  (concat (org-mutt:delimiter boundary mime-type) "\r\n" body-part))

(defun org-mutt:headers (boundary headers)
  (concat headers "\r\n"
          "Mime-Version: 1.0\r\n"
          "Content-Type: multipart/alternative; boundary=\"" boundary "\"\r\n"))

(defun org-mutt:org->html (org-str)
  "Export ORG-STR to HTML, assuming it is org format."
  (with-temp-buffer
    (insert org-str)
    (mark-whole-buffer)
    (org-html-convert-region-to-html)
    (buffer-string)))

(defun org-mutt:format-message (headers body)
  "Prepare an HTML message, given HEADERS and an org-mode string BODY."
  (let ((bound "=-=-="))
    (concat
     (org-mutt:headers bound headers)
     (org-mutt:encapsulation bound "text/plain" body)
     (org-mutt:encapsulation bound "text/html" (org-mutt:org->html body))
     (org-mutt:close-delimiter bound))))

(provide 'cb-mail)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-mail.el ends here
