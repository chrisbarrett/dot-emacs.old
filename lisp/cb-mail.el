;;; cb-mail.el --- Configuration for email

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
(require 'cb-modeline)
(autoload 'bbdb-record-name "bbdb")
(autoload 'std11-field-body "std11")
(autoload 'bbdb-complete-mail "bbdb-com")
(autoload 'bbdb-message-clean-name-default "bbdb-mua")
(autoload 'bbdb-record-mail "bbdb")
(autoload 'goto-address-find-address-at-point "goto-addr.el")
(autoload 'bbdb-records "bbdb")

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
                (cl-case major-mode
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

(defun cb-org:header->alist (str)
  "Extract the header values from the contents of the given buffer string STR."
  (->> (s-lines str)
    ;; Get header lines
    (--take-while (or (s-matches? (rx bol "#+" (* nonl) ":") it)
                      (s-blank? it)))
    (-remove 's-blank?)
    ;; Create alist of keywords -> values
    (--map (cl-destructuring-bind (_ key val &rest rest)
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
  (mark-whole-buffer)
  (org-html-convert-region-to-html)
  (let ((delete-trailing-lines t))
    (delete-trailing-whitespace))
  ;; Finish editing with the server.
  (save-buffer)
  (server-done))

;;;; Message initialisation

(defvar org-mutt:org-header "#+OPTIONS: toc:nil num:nil"
  "The default org header to insert into new messages.")

(defconst org-mutt:reply-quote-preamble
  (rx bol "On " (+ nonl) "wrote:" (* space) eol))

(defun org-mutt:quote-line? (ln)
  (s-matches?
   (rx (or
        ;; Quote?
        (group bol (+ ">"))
        ;; Quote header?
        ;;
        ;; HACK: Occasionally the sender address loses the
        ;; trialing space, so accept that in the regex.
        (group (or space ">") "wrote:" (* space) eol)))
   ln))

(defun org-mutt:rewrap-quoted-message (str)
  "Take the first quote level and discard nested quotes in STR.
Rewrap in an org-style quote block."
  (-if-let (preamble (car (s-match org-mutt:reply-quote-preamble str)))
    (cl-destructuring-bind (bod quote) (s-split (regexp-quote preamble) str)
      (s-unlines bod
                 preamble
                 "#+BEGIN_QUOTE"
                 ;; Reformat quote. Remove any inner quotes.
                 (->> quote
                   (s-lines)
                   (-map (~ s-chop-prefix "> "))
                   (-remove 'org-mutt:quote-line?)
                   (s-join "\n")
                   (s-trim))
                 "#+END_QUOTE"))
    str))

(defun org-mutt:prepare-message-buffer ()
  "Prepare a new message by reformatting the buffer."
  (let ((bod (buffer-string)))
    (atomic-change-group
      (delete-region (point-min) (point-max))
      (insert org-mutt:org-header)
      (newline 2)
      (unless (s-blank? bod)
        (save-excursion
          (newline 2)
          (insert (org-mutt:rewrap-quoted-message bod)))))))

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

;; Set the modeline description for mutt messages.

(defun org-mutt:buffer-description ()
  (when (and
         (buffer-file-name)
         (s-contains? ".mutt/temp/" (buffer-file-name)))
    (format "Unsent Message [mutt]")))

(add-to-list 'modeline-custom-description-functions 'org-mutt:buffer-description)

;; -----------------------------------------------------------------------------
;; Display unread mail count in mode-line.

(defvar cbm:mail-directory (f-join user-home-directory "Maildir")
  "The maildir to scan for new messages.")

(defvar cbm:mail-icon (create-image (f-join cb:assets-dir "letter.xpm")
                                    'xpm nil :ascent 'center))

(defvar cbm:mode-line-indicator nil
  "The entry to display in the modeline.")

(defun cbm:unread-mail-count ()
  "Return the number of unread messages in all folders in your maildir."
  (->> (f-directories cbm:mail-directory)
    (-mapcat 'f-directories)
    (-remove (~ s-matches? (rx (or "low" "archive" "draft" "org"
                                   "deleted" "trash" "sent"))))
    (-mapcat 'f-directories)
    (-filter (~ s-ends-with? "new"))
    (-map (C length f-files))
    (-sum)))

(defun cbm:make-indicator (n)
  (when (cl-plusp n)
    (concat
     (propertize "@" 'display cbm:mail-icon)
     (int-to-string n))))

(defun cbm:update-unread-count ()
  "Find the number of unread messages and update the modeline."
  (when (f-exists? cbm:mail-directory)
    (setq cbm:mode-line-indicator (cbm:make-indicator (cbm:unread-mail-count)))))

;; Use a combination of timers to update the modeline unread count.

(defvar cbm:unread-count-idle-timer
  (run-with-idle-timer 1 t 'cbm:update-unread-count))

(defvar cbm:unread-count-timer
  (run-with-timer 0 10 'cbm:update-unread-count))

(provide 'cb-mail)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-mail.el ends here
