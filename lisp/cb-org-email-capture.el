;;; cb-org-email-capture.el --- Capture tasks from emails in maildir

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130924.0129

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

;; Capture emails in a specified maildir folder with org-mode.

;;; Configuration:

;; Create an IMAP folder called 'org' and create a server-side rule to move messages
;; from your own address into that folder.
;;
;; Configure your offlineimaprc (or whatever you use to manage your maildir) to
;; sync that folder.
;;
;; This package works by associating message subjects with capture
;; templates. You should customise `org-capture-templates' to include actions
;; with the at least the following titles:
;;
;; * Todo
;; * Note
;; * Link
;; * Diary
;;
;; Furthermore, you should make an entry in `org-agenda-custom-commands' called
;; 'email' that should email a copy of your agenda to yourself.

;;; Usage:

;;
;; Once configured, you can send yourself an email and this package will perform
;; an appropriate action:
;;
;; * Messages whose body contains a link will be captured as a link.
;;
;; * Messages with 'diary' as the subject will be added to the diary.
;;
;; * Messages with 'agenda' as the subject will invoke a special 'Email'
;;   action. I have this configured to mail me a formatted copy of my agenda for
;;   the day.
;;
;; * In any other case, the message will be inserted at the appropriate tree for
;;   its capture template. If a capture template cannot be found, it will be
;;   inserted as a note using the 'Note' template.
;;
;; A message may contain special directives. They occur at the start of any line
;; and take the form <letter><space><args...>.
;;
;; * Schedule (s): The line is read as a time-stamp specification.
;;
;; * Deadline (d): The line is read as a time-stamp specification.
;;
;; * Tags (t): The line is interpreted as a space-separated list of tags.
;;
;; IMPORTANT: You should ensure that any messages in this maildir folder that
;; you do not want parsed and captured have subjects beginning with '[org]'
;; (without the quotes). This is to allow for other org commands to send
;; messages to your own address. For instance, my 'email' agenda command will
;; send an email to my own address and I don't want my agenda captured to a
;; note! I have a server-side rule that moves such messages into a separate
;; folder called 'org-messages'.

;;; Code:

(require 'cb-lib)
(require 'cb-paths)
(require 'async)
(require 'cb-org)
(autoload 'org-insert-link "org")
(autoload 'org-insert-time-stamp "org")
(autoload 'org-insert-subheading "org")
(autoload 'org-insert-todo-subheading "org")
(autoload 'org-read-date "org")
(autoload 'org-capture-goto-target "org-capture")
(autoload 'org-set-property "org")
(autoload 'org-set-tags-to "org")

;;; Customisable interface

;; IO FilePath
(defvar cbom:org-mail-folder
  (lambda ()
    (->> (f-join user-home-directory "Maildir")
      (f-directories)
      (-mapcat 'f-directories)
      (-first (~ s-ends-with? "org"))))
  "A function returning the maildir folder to capture from.")

;; IO FilePath
(defvar cbom:org-processed-mail-folder
  (lambda ()
    (->> (f-join user-home-directory "Maildir")
      (f-directories)
      (-mapcat 'f-directories)
      (-first (C (~ s-matches? (rx (or "trash" "deleted"))) -last-item
                 (~ s-split (f-path-separator))))))
  "A function returning the maildir folder to move items to once processed.")

;;; Internal

;; [FilePath] -> IO [(String, FilePath)]
(defun cbom:unprocessed-messages (dir)
  "The unprocessed mail in DIR.
DIR should be an IMAP maildir folder containing a subdir called 'new'."
  (let ((new (f-join dir "new")))
    (when (f-exists? new)
      (-map (π f-read-text I)
            (f-files new)))))

;;; Message parsing

;; String -> String -> Maybe String
(defun cbom:message-header-value (header msg)
  (cadr (s-match (eval `(rx bol ,header ":" (* space) (group (* nonl)))) msg)))

;; String -> Bool
(defun cbom:multipart-message? (msg)
  (s-matches? "multipart/alternative" (cbom:message-header-value "content-type" msg)))

;; String -> (String, String)
(defun cbom:split-message-head-and-body (msg)
  (AP (C (@ cons) (π (~ substring msg 0) (~ substring msg)))
      (s-index-of "\n\n" msg)))

;; String -> String
(defun cbom:multipart-body-plaintext-section (msg)
  (cl-destructuring-bind (head . body) (cbom:split-message-head-and-body msg)
    (->> body
      ;; Split the body by the boundary specified in the header and select
      ;; the section with plaintext MIME encoding.
      (s-split (cadr (s-match (rx "boundary=" (group (* nonl))) head)))
      (-first (~ s-contains? "text/plain"))
      ;; Tidy the section, dropping headers.
      (s-trim)
      (s-chop-suffix "--")
      (s-split "\n\n")
      (cadr)
      (s-trim)
      (s-chop-suffix "=")
      ;; Convert latin-1 line breaks.
      (s-replace "=\n" "")
      (s-replace "=0A" "\n"))))

;; String -> Maybe URI
(defun cbom:find-uri (str)
  "Extract the first URI from STR. Performs loose matching."
  (-when-let
      (uri (car (s-match
                 (rx bow
                     (or
                      ;; Match URIs, with and without protocol.
                      (and "http" (? "s") "://")
                      (and "www." (* alnum) ".")
                      ;; Loosely match strings with common domains.
                      (and (+ alnum) "."
                           (or "io" "edu" "net"
                               "gov" "com" "biz"
                               "org" "info" "co.")))
                     (* (not (any space "\n" "\r"))))
                 str)))
    ;; Set the URI's protocol to http if none is provided.
    (if (s-contains? "://" uri)
        uri
      (s-prepend "http://" uri))))

;; String -> String -> Maybe String
(defun cbom:match-directive (directive line)
  (cadr (s-match (eval `(rx bol ,directive (+ space) (group (* nonl)))) line)))

;; String -> TimeStringHM
(defun cbom:parse-12-hour-time (str)
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

;; String -> TimeStringHM
(defun cbom:parse-24-hour-time (str)
  (car (s-match (rx (** 1 2 digit) ":" (= 2 digit)) str)))

;; String -> TimeString
(defun cbom:parse-date (str)
  (when str
    ;; Try to extract a time of day from STR.
    (-if-let (time (or (cbom:parse-12-hour-time str)
                       (cbom:parse-24-hour-time str)))
        (format "%s %s" (org-read-date t nil str) time)
      (org-read-date t nil str))))

;; (String, FilePath) -> MessagePlist
(cl-defun cbom:parse-message ((msg path))
  "Parse message body."
  (let* ((body (if (cbom:multipart-message? msg)
                   (cbom:multipart-body-plaintext-section msg)
                 (cdr (cbom:split-message-head-and-body msg))))
         (lns (-remove 's-blank? (-map 's-trim (s-lines body))))
         (content (-remove (~ s-matches? (rx bol (or "s" "d" "t") (+ space))) lns))
         (scheduled (->> lns
                      (-keep (~ cbom:match-directive "s"))
                      (car)
                      (cbom:parse-date)))
         (subject (cbom:message-header-value "subject" msg))
         (uri (cbom:find-uri body)))
    ;; Construct a plist of parsed values.
    (list :uri uri
          :subject subject
          :title (car content)
          :notes (cdr content)
          :scheduled scheduled
          :filepath path

          :deadline (->> lns
                      (-keep (~ cbom:match-directive "d"))
                      (car)
                      (cbom:parse-date))
          :tags
          (->> lns
            (-keep (~ cbom:match-directive "t"))
            (-mapcat 's-split-words)
            (-distinct))
          ;; Now that the message body is entirely parsed we can determine what
          ;; form of data we're capturing. This allows the user to get away with
          ;; setting an empty subject most of the time.
          :kind
          (cond
           (uri "link")
           ((and (s-blank? subject) scheduled)
            "diary")
           ((s-blank? subject)
            "note")
           ((s-matches? (rx bol "book") subject)
            "reading")
           ((s-matches? (rx bol (or "song" "music" "album")) subject)
            "listening")
           ((s-matches? (rx bol (or "diary" "calendar" "appt" "appointment"))
                        subject)
            "diary")
           ((s-matches? (rx bol "agenda") subject)
            "agenda")

           (t
            (s-downcase subject))))))

(cl-defun cbom:validate-message-plist (&key uri kind &allow-other-keys)
  (when uri
    (cl-assert (equal "link" kind))))

;; Org capture
;;
;; Because capture templates may be interactive, we can't use them
;; directly. We do it a roundabout way instead, inspecting `org-capture-templates'
;; to find the insertion site corresponding to each parsed message.
;; This means the captured is unlikely to follow the exact format
;; specified by the capture template.
;;


;; IO [String]
(defun cbom:capture-keywords ()
  (-map (C s-downcase cadr) org-capture-templates))

;; MessagePlist -> IO Bool
(cl-defun cbom:capture-candidate? (&key kind &allow-other-keys)
  (-contains? (cbom:capture-keywords) kind))

;; FilePath
(defconst cbom:icon (f-join user-emacs-directory "assets" "org_unicorn.png"))

;; URI -> String
(defun cbom:maybe-download-title-at-uri (uri)
  "Download the title element."
  ;; Ignore URIs that may point to binary files, e.g. aac, wma, mpeg, pdf.
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

;; Env [String]
(defun cbom:todo-keywords ()
  (->> org-todo-keywords
    (-flatten)
    (-filter 'stringp)
    (-keep (C cadr (~ s-match (rx bol (group (+ word))))))
    (-map 's-upcase)))

;; String -> Env Bool
(defun cbom:starts-with-todo-keyword? (str)
  (-when-let (first-word (car (s-split-words str)))
    (-contains? (cbom:todo-keywords) (s-upcase first-word))))

;; [String] -> String
(defun cbom:format-notes (notes)
  (cond
   ((null notes) "")
   ((= (length notes) 1)
    (format "\n%s" (car notes))))
  (t
   (format "\n%s" (s-join "\n- " notes))))

;; MessagePlist -> Env String
(cl-defun cbom:format-for-insertion
    (&key kind uri title scheduled deadline notes &allow-other-keys)
  "Format a parsed message according to its kind."
  (cond

   ;; Messages with a URI are always captured as a link.
   (uri
    ;; Ensure the title doesn't contain square brackets.
    (let ((title (s-replace-all '(("[" . "(") ("]" . ")"))
                                (or (cbom:maybe-download-title-at-uri uri) uri))))
      (format "[[%s][%s]]" uri (s-truncate 70 title))))

   ;; Special diary format. The deadline is interpreted as an end time-stamp.
   ((equal "diary" kind)
    (concat (cond
             ((and scheduled deadline)
              (format "%s\n<%s>--<%s>" title scheduled deadline))
             (scheduled
              (format "%s\n<%s>" title scheduled))
             (t
              title))
            (cbom:format-notes notes)))

   ;; All other types can follow a standard style.
   (t
    (concat
     (cond
      ;; If this is a todo, ensure the headline starts with a todo keyword.
      ((and (equal "todo" kind) (not (cbom:starts-with-todo-keyword? title)))
       (concat "TODO " title))
      (t
       title))
     (when scheduled (format "\nSCHEDULED: <%s>" scheduled))
     (when deadline (format "\nDEADLINE: <%s>" deadline))
     (cbom:format-notes notes)))))

;; IO ()
(defun cbom:dispatch-agenda-email ()
  (let ((inhibit-redisplay t))
    (-when-let (key (->> org-agenda-custom-commands
                      (-first (& (C listp cdr) (C (~ s-matches? "email") cadr)))
                      (car)))
      (org-agenda nil key))))

;; MessagePlist -> IO ()
(cl-defun cbom:capture (str &key kind tags &allow-other-keys)
  "Read MSG-PLIST and execute the appropriate capture behaviour."

  ;; Move to the capture site associated with KIND.
  (cl-destructuring-bind (&optional key &rest rest_)
      (-first (C (~ equal kind) s-downcase cadr) org-capture-templates)
    (org-capture-goto-target (or key "n")))

  ;; Prepare headline.
  (end-of-line)
  (org-insert-subheading '(16))     ; 16 = at end of list

  ;; Insert item.
  (insert str)
  (org-set-tags-to tags)
  (org-set-property
   "CAPTURED"
   (s-with-temp-buffer
     (org-insert-time-stamp (current-time) t 'inactive))))

;; MessagePlist -> IO ()
(cl-defun cbom:remove-message (&key filepath &allow-other-keys)
  (when (f-exists? filepath)
    ;; Create filepath to the destination dir, with filename tags that mark
    ;; the message as read.
    (let* ((dest-file (format "%s:2,S" (car (s-split ":" (f-filename filepath)))))
           (dest-filepath (f-join (funcall cbom:org-processed-mail-folder)
                                  "cur"
                                  dest-file)))
      (f-move filepath dest-filepath))))

;; MessagePlist -> IO ()
(cl-defun cbom:growl (&key kind title &allow-other-keys)
  (growl (format "%s Captured" (s-capitalize kind))
         (s-truncate 40 title)
         cbom:icon))

;; IO [FilePath]
(defvar cbom:processed-messages nil
  "A list of messages that have already been processed.
This is needed to prevent double-ups in the case that the timer
repeats before all the messages are processed and removed.")

;; IO ()
(defun cbom:capture-messages ()
  "Parse and capture unread messages in `cbom:org-mail-folder'.
Captures messages subjects match one of the values in `org-capture-templates'.
Captured messages are marked as read."
  (interactive)
  ;; Get a list of unprocessed messages. Check for membership in the processed
  ;; messages list to prevent double-ups.
  (let ((msgs (->> (AP cbom:org-mail-folder)
                (cbom:unprocessed-messages)
                (-remove (C (~ -contains? cbom:processed-messages) cdr))
                (-map 'cbom:parse-message))))
    (--each msgs (add-to-list 'cbom:processed-messages (plist-get it :filepath)))

    ;; Capture each message.
    (dolist (pl msgs)
      ;; Capture according to kind.
      (cond
       ;; If a message contains a URI, capture using the link template.
       ((equal "agenda" (plist-get pl :kind))
        (cbom:dispatch-agenda-email)
        (growl "Agenda Emailed" "" cbom:icon))
       ;;
       ;; Prepare messages for capture in another Emacs process. This keeps
       ;; the UI responsive while performing web requests, etc.
       (t
        (async-start
         `(lambda ()
            ,(async-inject-variables "^pl$")
            ,(async-inject-variables "load-path")
            (package-initialize)
            (require 'cb-org-email-capture)
            (list pl (apply 'cbom:format-for-insertion pl)))
         (lambda+ ((pl fmt))
           (save-excursion
             (save-window-excursion
               ;; The user may have interactively changed the default notes
               ;; file, so we rebind it to the note file set at init time.
               (let ((org-default-notes-file org-init-notes-file))
                 (apply 'cbom:capture fmt pl)
                 (apply 'cbom:growl pl)
                 (apply 'cbom:remove-message pl)))))))))))

;;; Timer

(hook-fn 'after-init-hook
  (defvar cbom:capture-timer
    (run-with-timer 5 60 (lambda ()
                           (when (featurep 'org)
                             (with-demoted-errors
                               (cbom:capture-messages)))))))

(provide 'cb-org-email-capture)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-org-email-capture.el ends here
