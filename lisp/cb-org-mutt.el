;;; cb-org-mutt.el --- Define custom mode for mutt message composition.

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

;; Define custom mode for mutt message composition.

;;; Code:

(require 'org-mime)
(require 's)
(require 'dash)

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
      (insert org-mime-default-header)
      (newline)
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

(provide 'cb-org-mutt)

;;; cb-org-mutt.el ends here
