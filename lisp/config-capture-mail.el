;;; config-capture-mail.el --- Configure email capture with orgmode

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

;; Configure email capture with orgmode

;;; Code:

(require 'utils-common)
(require 'capture-mail)
(require 'config-darwin)
(require 'config-orgmode)

(let ((account-dir (--first
                    (not (s-starts-with? "." (f-filename it)))
                    (f-directories user-mail-directory))))
  (setq cm-archived-messages-dir (f-join account-dir "org" "cur")
        cm-capture-messages-dir  (f-join account-dir "org" "new")))

(cl-defun cm--growl (kind desc)
  (growl (format "%s Captured" (s-capitalize kind))
         (s-truncate 40 desc)
         (f-join user-emacs-directory "assets" "org_unicorn.png")))

(cl-defun cm--org-capture (str template-name &optional tags)
  "Capture STR with org-mode.
TEMPLATE-NAME is a string matching the name of a capture
template."
  (let ((org-default-notes-file org-init-notes-file))
    (save-excursion
      (save-window-excursion
        (when (boundp 'org-capture-templates)
          (cl-destructuring-bind (&optional key name file tree template &rest rest)
              (-first (C (~ equal template-name) s-downcase cadr)
                      org-capture-templates)
            (org-capture-goto-target (or key "n"))
            ;; Prepare headline.
            (end-of-line)
            (org-insert-heading '(16))   ; 16 = at end of list
            ;; Insert item.
            (insert str)
            (org-set-tags-to tags)))))))

(setq cm-default-parser
      (list :type 'note
            :predicate '-true-fn
            :parser (~ cm-value 'body)
            :handler
            (lambda (str)
              (cm--org-capture str "note")
              (cm--growl "Note" str))))

(defun cm--find-uri-in-body (alist)
  "Extract the first URI from the body in ALIST. Performs loose matching."
  (-when-let (str (cm-value 'body alist))
    (-when-let
        (uri (car (s-match
                   (rx bow
                       (or
                        ;; Match URIs, with and without protocol.
                        (and "http" (? "s") "://")
                        (and "www." (* alnum) ".")
                        ;; Loosely match strings with common domains.
                        (and (+? (not (any "\r" "\n" space))) "."
                             (or "io" "edu" "net"
                                 "gov" "com" "biz"
                                 "org" "info" "co.")))
                       (* (not (any space "\n" "\r"))))
                   str)))
      ;; Set the URI's protocol to http if none is provided.
      (if (s-contains? "://" uri)
          uri
        (s-prepend "http://" uri)))))

(cm-declare-message-parser 'link
  :predicate 'cm--find-uri-in-body
  :parser 'cm--find-uri-in-body
  :handler
  (lambda (uri)
    (async-start
     `(lambda ()
        (package-initialize)
        (require 's)
        (require 'dash)
        (let* ((uri ,uri)
               (downloaded-title
                ;; Fetch webpage and download title.
                (unless (s-matches? (rx "." (or "z" "r" "t" "p" "d" "a" "w" "m")
                                        (** 2 3 alnum) eol)
                                    uri)
                  (with-timeout (10 nil)
                    (ignore-errors
                      (with-current-buffer
                          (url-retrieve-synchronously
                           (if (s-matches? (rx "http" (? "s") "://") uri)
                               uri
                             (s-prepend "http://" uri)))
                        ;; Clear request status.
                        (message nil)
                        (cadr (s-match (rx "<title>" (group (* nonl)) "</title>")
                                       (buffer-string))))))))
               ;; Escape chars used by org titles.
               (title (s-replace-all '(("[" . "(") ("]" . ")"))
                                     (or downloaded-title uri))))
          (list :uri uri
                :str (format "[[%s][%s]]" uri (s-truncate 70 title))
                :title title)))

     (lambda+ ((&key uri title str))
       (cm--org-capture str "link")
       (cm--growl "Link" (or title uri))))))

(cm-declare-message-parser 'diary
  :predicate (~ cm-matches? (rx bol "diary") 'subject)
  :parser (lambda (alist)
            (cl-destructuring-bind (header date &rest notes)
                (s-split (rx (or "\n" (group bow (or "next" "on") eow)))
                         (cm-value 'body alist))
              (format "%s\n<%s>%s"
                      header
                      (org-read-date nil nil (or date "."))
                      (s-join "\n" notes))))
  :handler (lambda (str)
             (cm--org-capture str "diary")
             (cm--growl "Appointment" str)))

(defun cm--parse-12-hour-time (str)
  (cl-destructuring-bind (&optional _ hour min ampm &rest rest_)
      (s-match (rx (group (** 1 2 digit))
                   (? ":" (group (= 2 digit)))
                   (group (or "am" "pm")))
               str)
    (when hour
      (format "%s:%s"
              ;; Convert to 24-hour. Get the modulo just to prevent crazy times.
              (if (s-matches? "pm" ampm)
                  (mod (+ 12 (string-to-number hour))
                       24)
                hour)
              (or min "00")))))

(defun cm--parse-24-hour-time (str)
  (car (s-match (rx (** 1 2 digit) ":" (= 2 digit)) str)))

(defun cm--parse-date (str)
  (when str
    ;; Try to extract a time of day from STR.
    (-if-let (time (or (cm--parse-12-hour-time str)
                       (cm--parse-24-hour-time str)))
        (format "%s %s" (org-read-date t nil str) time)
      (org-read-date t nil str))))

(defun cm--match-directive (directive line)
  (cadr (s-match (eval `(rx bol ,directive (+ space) (group (* nonl)))) line)))

(defun cm--parse-task (alist)
  (let* ((todo-kw (cm-value 'subject alist))
         (lns (s-split "\n" (cm-value 'body alist)))
         (content
          (-remove (~ s-matches? (rx bol (or "s" "d" "t") (+ space))) lns))
         (header (car content))
         (notes (cdr content))

         (scheduled
          (->> lns
            (-keep (~ cm--match-directive "s"))
            car
            cm--parse-date))

         (deadline
          (->> lns
            (-keep (~ cm--match-directive "d"))
            car
            cm--parse-date))
         (tags
          (->> lns
            (-keep (~ cm--match-directive "t"))
            (-mapcat 's-split-words)
            (-distinct)))
         )
    (list :str
          (concat (s-upcase todo-kw) " " header
                  (when scheduled (format "\nSCHEDULED: <%s>" scheduled))
                  (when deadline  (format "\nDEADLINE: <%s>" deadline))
                  (cond
                   ((null notes) "")
                   ((= (length notes) 1)
                    (format "\n%s" (car notes)))
                   (t
                    (format "\n%s" (s-join "\n- " notes)))))

          :kind todo-kw
          :tags tags)))

(cm-declare-message-parser 'task
  :predicate
  (~ cm-matches? (rx bol (or "todo" "next" "maybe" "someday") (* space) eol)
     'subject)
  :parser 'cm--parse-task
  :handler
  (lambda+ ((&key str kind tags))
    (cond
     ((s-matches? (rx (or "next" "todo")) kind)
      (cm--org-capture str "todo" tags))
     ((s-matches? (rx (or "maybe" "someday")) kind)
      (cm--org-capture str "someday" tags))
     (t
      (error "No template for kind: %s" kind)))

    (cm--growl "Task" str)))

(unless noninteractive
  (hook-fn 'after-init-hook
    (defvar cm-capture-timer
      (when (f-exists? cm-capture-messages-dir)
        (run-with-timer 5 60 (lambda ()
                               (capture-mail cm-capture-messages-dir)))))))

(defun cb-org:quick-capture (type body)
  "Use the capture-mail functionality to capture something quickly.

TYPE is the type of item.  It is ordinarily the subject for emails.

BODY is the string to interpret."
  (interactive "s[Quick Capture] Type: \ns[Quick Capture] String: ")
  (cl-loop
   with parsers = (-concat cm--parsers (list cm-default-parser))
   with alist = `((subject . ,type)
                  (body . ,body)
                  (date . (format-time-string "%FT%T%z"))
                  )
   for p in parsers do
   (cl-destructuring-bind (&key type predicate parser handler) p
     (when (funcall predicate alist)
       (-when-let
           (parsed-val
            (condition-case-unless-debug _
                (funcall parser alist)
              (error
               (display-warning 'capture-mail "Failed to parse input."))))
         (cl-return (cons type (funcall handler parsed-val))))))))

(add-to-list 'org-action-picker-options
             '("q" "Quick Capture" cb-org:quick-capture))


(provide 'config-capture-mail)

;;; config-capture-mail.el ends here
