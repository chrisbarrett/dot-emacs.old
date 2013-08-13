;;; osx-contacts.el --- Import OS X Address Book to bbdb.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130813.0007

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

;; Import OS X Address Book into BBDB. Requires the `contacts' program.

;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)
(autoload 'bbdb-record-name "bbdb")
(autoload 'bbdb-record-mail "bbdb")
(autoload 'bbdb-create-internal "bbdb-com")
(autoload 'bbdb-records "bbdb")
(autoload 'bbdb-save "bbdb")

(defun osxc/contacts-to-string ()
  "Call the contacts program.  Return a formatted string representing the user's contacts."
  (shell-command-to-string
   (format "contacts -H -l -f '%s'"
           (s-join "\t"
                   '(
                     ;; Names
                     "%n"                  ; last name
                     "%c"                   ; company
                     "%nn"                  ; nickname
                     ;; email
                     "%he"                  ; home
                     "%we"                  ; work
                     "%oe"                  ; other
                     ;; phone
                     "%hp"                  ; home
                     "%mp"                  ; mobile
                     "%Mp"                  ; main
                     "%wp"                  ; work
                     )))))

(defun* osxc/parse-card
    ((&optional
      name company aka
      home-email work-email other-email
      home-phone mobile-phone main-phone work-phone))
  "Parse an individual card fields list into the format expected `bbdb-create-internal'."

  ;; BBDB will bork if it sees empty strings, so define some helper methods that
  ;; translate empty strings to null values.
  (cl-flet ((maybe (s) (unless (s-blank? s) s))
            (maybe-vec (label s) (unless (s-blank? s) (vector label s)))
            (non-null (&rest xs) (-remove 'null xs)))
    (list
     name
     nil ; no affix
     (non-null (maybe aka))
     (non-null (maybe company))
     (non-null (maybe home-email) (maybe work-email) (maybe other-email))
     (non-null (maybe-vec "home" home-phone)
               (maybe-vec "mobile" mobile-phone)
               (maybe-vec "main" main-phone)
               (maybe-vec "work" work-phone)))))

(defun osxc/parse-contacts (contacts-shell-output)
  "Parse the output from contacts into a form usable by `bbdb-create-internal'.
CONTACTS-SHELL-OUTPUT is the result from `osxc/contacts-to-string'."
  (->> contacts-shell-output
    ;; Each line represents a card.
    (s-split "\n")
    ;; Split each line into fields
    (--map (-map 's-trim (s-split "\t" it)))
    ;; BBDB requires that names can be split into first+last. Filter degenerate
    ;; cards that don't conform to this.
    (--filter (let ((name (car it)))
                (equal 2 (length (s-split-words name)))))
    ;; Parse individual cards.
    (-map 'osxc/parse-card)))

(defun osxc/bbdb-contains-record? (record)
  "Check whether BBDB contains an entry with the name name or email address as RECORD."
  (destructuring-bind (&optional name _affix _company _aka mails &rest rest) record
    (--any? (or (equal (bbdb-record-name it) name)
                (-intersection (bbdb-record-mail it) mails))
            (bbdb-records))))

(defun import-osx-contacts-to-bbdb (&optional quiet)
  "Import contacts from the OS X address book to BBDB.
When QUIET is non-nil, do not print summary of added items."
  (interactive "P")
  (require 'bbdb)
  (let ((counter 0))
    ;; Import contacts.
    (--each (osxc/parse-contacts (osxc/contacts-to-string))
      (unless (osxc/bbdb-contains-record? it)
        (apply 'bbdb-create-internal it)
        (incf counter)))
    ;; Clean up.
    (bbdb-save)
    (unless quiet
      (message "%s %s added to BBDB" counter
               (if (= 1 counter) "contact" "contacts")))))

(provide 'osx-contacts)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; osx-contacts.el ends here
