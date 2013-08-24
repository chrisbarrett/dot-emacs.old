;;; cb-org.el --- Configuration for org

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0014

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

;; Configuration for `org-mode'.  Org mode is for keeping notes, maintaining
;; TODO lists, planning projects, and authoring documents with a fast and
;; effective plain-text system.  See:
;;
;;    http://orgmode.org/
;;

;;; Code:

(require 'use-package)
(require 'cb-foundation)
(require 'cb-mode-groups)
(require 'noflet)
(require 'cb-lib)
(require 'cb-evil)
(autoload 'bbdb-complete-mail "bbdb-com")
(autoload 'bbdb-message-clean-name-default "bbdb-mua")
(autoload 'bbdb-record-mail "bbdb")
(autoload 'bbdb-record-name "bbdb")
(autoload 'bbdb-records "bbdb")
(autoload 'goto-address-find-address-at-point "goto-addr.el")
(autoload 'message-send-and-exit "message")
(autoload 'projectile-project-buffer-names "projectile")
(autoload 'projectile-project-name "projectile")
(autoload 'projectile-project-p "projectile")

;; Declare org-related values before org-mode is loaded.
;; This is mainly to reduce compiler warnings.
(defvar org-directory (f-join user-home-directory "org"))
(defvar org-mobile-inbox-for-pull (f-join org-directory "mobile.org"))
(defvar org-mobile-directory (f-join user-dropbox-directory "Apps" "MobileOrg"))
(defvar org-default-notes-file (f-join org-directory "notes.org"))
(defvar org-id-locations-file (f-join cb:tmp-dir "org-id-locations"))
(defvar org-clock-persist-file (f-join user-dropbox-directory ".org-clock-save.el"))
(defvar org-export-publishing-directory (f-join user-home-directory "Desktop"))
(defvar org-agenda-diary-file (f-join org-directory "diary.org"))
(defvar org-export-exclude-tags '("noexport" "crypt"))
(defvar calendar-date-style 'european)

;; Add contrib directory to load-path.
(add-to-list 'load-path (f-join cb:etc-dir "org-mode" "contrib" "lisp"))

;; Show org agenda with M-O.
(declare-modal-executor org-agenda-fullscreen
  :bind "M-O"
  :command (org-agenda-list prefix-arg nil 1))

;; If we're running in a graphical context, show the agenda on startup.
(when (or (daemonp) (display-graphic-p))
  (hook-fn 'after-init-hook
    (executor:org-agenda-fullscreen)))

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
      ;; Prepare for user interaction.
      (insert (format "#+TO: %s\n#+SUBJECT: %s\n\n" to subject))
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
      (let ((org-export-with-toc nil))
        (org-mime-htmlize nil))
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

;; Add command to yank region as quote.

(defun cb-org:yank-region-as-quote (beg end)
  (interactive "r")
  (if (region-active-p)
      (progn
        (kill-new (cb-org:buffer-substring-to-quote beg end))
        (deactivate-mark)
        (message "Region yanked as quote."))
    (error "No region is active, so no quote could be yanked")))

(global-set-key (kbd "C-c y") 'cb-org:yank-region-as-quote)

;; `org-mode' is a suite of editing and management tools centred around
;; human-readable text files.
(use-package org
  :defer t
  :init
  (progn
    (define-prefix-command 'cb-org-map)

    (after 'evil
      (evil-global-set-key 'normal (kbd "C-o") 'cb-org-map)
      (evil-global-set-key 'visual (kbd "C-o") 'cb-org-map))

    (bind-keys
      :overriding? t
      :map cb-org-map
      "C-o" 'cb-org-map
      "C-o C-l" 'org-store-link
      "C-o C-b" 'org-iswitchb
      "C-o C-a" 'org-agenda
      "C-o C-f" 'org-search-view
      "C-o C-j" 'org-clock-goto
      "C-o d" (command (find-file org-agenda-diary-file))
      "C-o K" (command (org-capture nil "T"))
      "C-o k" (command (org-capture nil "t"))
      "C-o n" (command (find-file org-default-notes-file))

      "C-o p" (command
               (let ((name (projectile-project-name)))
                 (find-file (cb-org:project-file))
                 (cb-org:prepare-project-file name)
                 (when (bobp)
                   (cb-org:skip-headers))))

      "C-o C-k" 'org-capture)

    (declare-modal-executor org-show-todo-list
      :bind "C-o <RET>"
      :command (progn
                 (org-agenda prefix-arg "t")
                 (org-agenda-filter-apply '("-someday") 'tag)))

    (declare-modal-executor org-show-filtered-todo-list
      :bind "C-o t"
      :command (org-agenda prefix-arg "T"))

    (declare-modal-executor org-tags-view-todos-fullscreen
      :bind "C-o v"
      :command (org-tags-view t))

    (declare-modal-executor org-tags-view-all-fullscreen
      :bind "C-o C-v"
      :command (org-tags-view nil)))

  :config
  (progn
    (setq
     ;; General config

     org-modules '(org-bbdb org-crypt org-w3m org-habit)
     org-completion-use-ido t
     org-startup-indented t
     org-enforce-todo-dependencies t
     org-cycle-separator-lines 0
     org-log-into-drawer t
     org-log-done 'time
     org-reverse-note-order nil
     org-link-mailto-program (quote (compose-mail "%a" "%s"))
     org-return-follows-link t
     org-indirect-buffer-display 'current-window
     org-blank-before-new-entry nil
     org-footnote-auto-adjust t
     org-put-time-stamp-overlays t

     ;; Refiling

     org-refile-targets '((nil :maxlevel . 9)
                          (org-agenda-files :maxlevel . 9))
     org-refile-use-outline-path t
     org-outline-path-complete-in-steps nil
     org-refile-allow-creating-parent-nodes 'confirm
     ;; Exclude todo keywords with a done state from refile targets.
     org-refile-target-verify-function
     (lambda () (not (member (nth 2 (org-heading-components)) org-done-keywords)))

     ;; Projects

     org-stuck-projects '("+project&TODO={.+}/-DONE-CANCELLED"
                          ("NEXT" "TODO") nil "\\<IGNORE\\>")

     ;; Tags

     org-tag-persistent-alist
     '(("finance" . ?f)
       ("hold" . ?H)
       ;; Financial tags
       (:startgroup . nil)
       ("debt" . ?d)
       ("reimbursement" . ?r)
       (:endgroup . nil)
       ;; Context tags
       (:startgroup . nil)
       ("@anywhere" . ?a)
       ("@errand" . ?e)
       ("@leisure" . ?l)
       ("@home" . ?h)
       ("@work" . ?w)
       (:endgroup . nil)))

    ;; Set some default effort times.
    (add-to-list 'org-global-properties
                 `("Effort_ALL" .
                   ,(concat "1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00 9:00 "
                            "0:05 0:10 0:30")))

    (evil-define-keys 'normal org-mode-map
      "<return>" 'org-return
      "M-P" 'outline-previous-visible-heading
      "M-N" 'outline-next-visible-heading
      "SPC" 'org-cycle
      "z m" (command (save-excursion
                       (goto-char (point-min))
                       (org-global-cycle 1)
                       (recenter)))
      "z r" (command (save-excursion
                       (goto-char (point-min))
                       (org-global-cycle 3)
                       (recenter))))

    (defun cb-org:ctrl-c-ctrl-k (&optional n)
      "Kill subtrees, unless we're in a special buffer where it should cancel."
      (interactive "p")
      (if (s-starts-with? "*Org" (buffer-name))
          (org-kill-note-or-show-branches)
        (org-cut-subtree n)))

    (define-keys org-mode-map
      "C-c C-k" 'cb-org:ctrl-c-ctrl-k
      "C-c C-." 'org-time-stamp-inactive
      "M-p" 'org-metaup
      "M-n" 'org-metadown
      ;; disable annoying comment toggle key
      "C-c ;" nil)

    ;; Hooks

    ;; Enter insert state for popup notes.
    (hook-fn 'org-mode-hook
      (when (and (equal (buffer-name) "*Org Note*"))
        (cb:append-buffer)))

    ;; Diminish org minor modes.
    (hook-fn 'cb:org-minor-modes-hook
      (--each cb:org-minor-modes
        (ignore-errors (diminish it))))

    (defun tidy-org-buffer ()
      "Perform cosmetic fixes to the current org-mode buffer."
      (org-table-map-tables 'org-table-align 'quiet)
      ;; Realign tags.
      (org-set-tags 4 t)
      ;; Remove empty properties drawers.
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward ":PROPERTIES:" nil t)
          (save-excursion
            (org-remove-empty-drawer-at "PROPERTIES" (match-beginning 0))))))

    (hook-fn 'org-mode-hook
      (add-hook 'before-save-hook 'tidy-org-buffer nil t))

    ;; Sub-task completion triggers parent completion.
    (hook-fn 'org-after-todo-statistics-hook
      :arglist (n-done n-not-done)
      (let (org-log-done org-log-states) ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

    ;; Auto-save notes file
    ;;
    ;; This has the nice side-effect that encrypted regions will be
    ;; automatically re-encrypted after a period of inactivity.

    (defvar cb-org:notes-save-idle-delay 40)

    (defvar cb-org:notes-save-timer
      (unless noninteractive
        (run-with-idle-timer cb-org:notes-save-idle-delay t 'cb-org:save-notes))
      "Timer that automatically saves the notes buffer on idle.")

    (defun cb-org:save-notes ()
      "Save the notes file."
      (-when-let (buf (--first-buffer (equal (buffer-file-name it)
                                             org-default-notes-file)))
        (with-current-buffer buf
          (when (buffer-modified-p)
            (save-buffer)))))

    ;; Org babel

    (setq org-src-fontify-natively t
          org-confirm-babel-evaluate nil)

    (org-babel-do-load-languages
     'org-babel-load-languages '((python . t)
                                 (C . t)
                                 (calc . t)
                                 (emacs-lisp . t)
                                 (ruby . t)
                                 (clojure . t)
                                 (haskell . t)))

    ;; Org config

    (defface org-todo-next
      '((((background dark))
         (:foreground "OrangeRed1" :bold t))
        (((background light))
         (:foreground "OrangeRed4" :bold t))
        (t
         (:inherit org-todo)))
      "Face for todos with the NEXT label."
      :group 'org-faces)

    (setq org-catch-invisible-edits 'smart
          org-pretty-entities t

          org-todo-keywords
          '((sequence
             "TODO(t)" "NEXT(n!)" "MAYBE(m!)"
             "OUTSTANDING(o)" "WAITING(w@/!)" "APPT(a!)"
             "|"
             "DONE(d!)" "PAID(p!)" "VOID(v@)" "CANCELLED(c@)" "DELEGATED(D@)"))

          org-todo-keyword-faces '(("NEXT" . org-todo-next)))

    (--each '("NOTES" "COMMENTS" "PROPERTIES" "LOGBOOK")
      (add-to-list 'org-drawers it))

    (add-hook 'org-mode-hook 'auto-revert-mode)

    (defadvice org-add-log-note (before exit-minibuffer activate)
      "Prevent attempts to expand the minibuffer."
      (when (minibufferp (window-buffer (selected-window)))
        (other-window 1)))))

;; Define pairs for org-mode blocks.
(after 'smartparens
  (sp-with-modes '(org-mode)
    (sp-local-pair "#+BEGIN_SRC" "#+END_SRC")
    (sp-local-pair "#+begin_src" "#+end_src")
    (sp-local-pair "#+BEGIN_EXAMPLE" "#+END_EXAMPLE")
    (sp-local-pair "#+begin_example" "#+end_example")
    (sp-local-pair "#+BEGIN_VERSE" "#+END_VERSE")
    (sp-local-pair "#+begin_verse" "#+end_verse")))

;; Use org-mode-style tables and structure editing in message-mode.
(after 'message
  (add-hook 'message-mode-hook 'orgstruct++-mode)
  (add-hook 'message-mode-hook 'orgtbl-mode)
  (define-key message-mode-map (kbd "C-c RET RET") 'org-ctrl-c-ret))

;; Configure org's sub-features only if org-mode is actually loaded.
(after 'org

  ;; Commands for formatting project file.

  (defun cb-org:project-file ()
    "Get the path to the project file for the current project."
    (or
     ;; If we're capturing, check if we were looking at a project when the capture
     ;; was started. Return that file if so.
     (let ((prev-buf (nth 1 (buffer-list))))
       (and (equal "*Capture*" (buffer-name))
            (s-ends-with? ".project.org" (buffer-name prev-buf))
            (buffer-file-name prev-buf)))
     ;; Otherwise, compute the path to a project file based on the current path.
     (let ((name (cond
                  ((projectile-project-p) (projectile-project-name))
                  ((projectile-project-buffer-names) (car (projectile-project-buffer-names)))
                  (t (error "Not in a project")))))
       (f-join org-directory
               (concat (s-alnum-only name) ".project.org")))))

  (defun cb-org:ensure-field (kvp &optional permissive?)
    "Insert metadata key-value pair KVP at point.
No insertion is done if the field is already set in the current
buffer.  When PERMISSIVE is set, allow duplicate instances of the
given key.
Non-nil if the field was inserted."
    (destructuring-bind (key . val) (s-split (rx space) kvp)
      (unless (s-matches? (rx-to-string
                           (if permissive?
                               `(and bol ,key (* space) ,(or (car val) ""))
                             `(and bol ,key (* space))))
                          (buffer-string))
        (insert (concat kvp "\n"))
        'inserted)))

  (defun cb-org:prepare-project-file (project-name)
    "Ensure the current project file has its metadata fields set.
Non-nil if modifications where made."
    ;; Make this task file show up in org buffers.
    (if (and (f-exists? (buffer-file-name))
             (boundp 'org-agenda-files))
        (add-to-list 'org-agenda-files (buffer-file-name))
      (eval `(hook-fn 'after-save-hook
               :local t
               (add-to-list 'org-agenda-files ,(buffer-file-name)))))
    ;; Set metadata fields.
    (save-excursion
      (goto-char (point-min))
      ;; Skip file variables.
      (when (emr-line-matches? (rx "-*-" (* nonl) "-*-"))
        (forward-line))
      (beginning-of-line)
      ;; Set fields
      (cb-org:ensure-field (concat "#+TITLE: " project-name))
      (cb-org:ensure-field (concat "#+AUTHOR: " user-full-name))
      (cb-org:ensure-field "#+STARTUP: lognotestate" t)
      (cb-org:ensure-field "#+STARTUP: lognotedone" t)
      (cb-org:ensure-field "#+DESCRIPTION: Project-level notes and todos")
      (cb-org:ensure-field (concat "#+CATEGORY: " project-name))
      (cb-org:ensure-field (format "#+FILETAGS: :%s:" (s-alnum-only project-name)))))

  (defun cb-org:skip-headers ()
    "Move point past the header lines of an org document."
    (while (and (or (emr-line-matches? (rx bol (or "#+" "-*-"))) (emr-blank-line?))
                (not (save-excursion (forward-line) (eobp))))
      (forward-line)))

  ;; `org-capture' is used to interactively read items to be inserted into
  ;; org-mode buffers.
  (use-package org-capture
    :config
    (progn

      (defun* cb-org:read-string-with-file-ref
          (&optional (prompt "TODO")
                     (file (cb-org:project-file)))
        "Prompt the user for string, with reference to a file."
        (read-string (format "%s [%s]: " prompt (f-short file))
                     nil t))

      ;; Enter insertion mode in capture buffer.
      (hook-fn 'org-capture-mode-hook
        (when (fboundp 'evil-append-line)
          (evil-append-line 1)))

      (add-hook 'org-capture-before-finalize-hook 'indent-buffer 'append)

      (setq org-capture-templates
            `(("t" "Todo" entry
               (file+headline org-default-notes-file "Tasks")
               ,(s-unlines
                 (concat "* TODO %^{Description}%?    "
                         ":%^{Context|@anywhere|@errand|@leisure|@home|@project|@work}:")
                 "SCHEDULED: %^{Schedule}t"
                 ":LOGBOOK:"
                 ":CAPTURED: %U"
                 ":END:")
               :clock-in t
               :clock-resume t)

              ("s" "Someday" entry
               (file+olp org-default-notes-file "Someday" "Tasks")
               ,(s-unlines
                 "* MAYBE %^{Description}%?"
                 ":LOGBOOK:"
                 ":CAPTURED: %U"
                 ":END:")
               :clock-in t
               :clock-resume t)

              ("d" "Diary" entry
               (file+datetree org-agenda-diary-file)
               "* %?\n%^t"
               :clock-resume t)

              ("b" "Bill" entry
               (file+headline org-default-notes-file "Bills")
               ,(s-unlines
                 "* TODO %^{Description}"
                 "DEADLINE: %^{Deadline}t"
                 ":LOGBOOK:"
                 ":CAPTURED: %U"
                 ":END:")
               :immediate-finish t
               :clock-resume t)

              ("h" "Habit" entry
               (file+headline org-default-notes-file "Habits")
               ,(s-unlines
                 (concat "* TODO %^{Description}%?    "
                         ":%^{Context|@anywhere|@errand|@leisure|@home|@project|@work}:")
                 "SCHEDULED: %^{Schedule}t"
                 ":PROPERTIES:"
                 ":STYLE: habit"
                 ":END:"
                 ":LOGBOOK:"
                 ":CAPTURED: %U"
                 ":END:")
               :clock-resume t)

              ("r" "Reading" entry
               (file+olp org-default-notes-file "Someday" "Readings")
               ,(s-unlines
                 "* TODO %^{Title}"
                 ":LOGBOOK:"
                 ":CAPTURED: %U"
                 ":END:")
               :immediate-finish t
               :clock-resume t)

              ("l" "Link" entry
               (file+headline org-default-notes-file "Links")
               ,(s-unlines
                 "* %a%?"
                 ":LOGBOOK:"
                 ":CAPTURED: %U"
                 ":END:")
               :immediate-finish t
               :clock-resume t)

              ("m" "Listening" entry
               (file+olp org-default-notes-file "Someday" "Listening")
               ,(s-unlines
                 "* TODO %^{Title}"
                 ":LOGBOOK:"
                 ":CAPTURED: %U"
                 ":END:")
               :immediate-finish t
               :clock-resume t)

              ("n" "Note" entry
               (file+headline org-default-notes-file "Notes")
               ,(s-unlines
                 "* %i%?"
                 ":LOGBOOK:"
                 ":CAPTURED: %U"
                 ":END:")
               :clock-resume t)

              ("z" "Task Note" entry
               (clock)
               ,(s-unlines
                 "* %i%?"
                 ":LOGBOOK:"
                 ":CAPTURED: %U"
                 ":END:")
               :clock-keep t
               :kill-buffer t)

              ("L" "Task Link" entry
               (clock)
               ,(s-unlines
                 "* %a%?"
                 ":LOGBOOK:"
                 ":CAPTURED: %U"
                 ":END:")
               :clock-keep t
               :kill-buffer t)

              ("T" "Project Task" entry
               (file+headline (cb-org:project-file) "Tasks")
               ,(s-unlines
                 "* TODO %(cb-org:read-string-with-file-ref)%?"
                 "SCHEDULED: %^{Schedule}t"
                 ":LOGBOOK:"
                 ":CAPTURED: %U"
                 ":END:")
               :kill-buffer t
               :clock-resume t
               :clock-in t)

              ("N" "Project Note" entry
               (file+headline (cb-org:project-file) "Notes")
               ,(s-unlines
                 "* %i%?"
                 ":LOGBOOK:"
                 ":CAPTURED: %U"
                 ":END:")
               :clock-keep t
               :kill-buffer t)))))

  ;; `org-agenda' provides an interactive journaling system, collecting
  ;; information from org-mode buffers.
  (use-package org-agenda
    :config
    (progn
      ;; Show today's agenda after a period of inactivity.

      (defvar cb-org:show-agenda-idle-delay (* 7 60)
        "The delay in seconds after which to pop up today's agenda.")

      (defvar cb-org:show-agenda-idle-timer
        (unless noninteractive
          (run-with-idle-timer cb-org:show-agenda-idle-delay t
                               (lambda () (org-agenda-list nil nil 1))))
        "Idle timer that will display today's org agenda as a pop-up.
See `cb-org:show-agenda-idle-delay'.")


      (add-hook 'org-agenda-mode-hook 'org-agenda-to-appt)
      (add-to-list 'org-agenda-files org-directory)

      (setq org-agenda-insert-diary-extract-time t
            org-agenda-span 'week
            org-agenda-skip-deadline-if-done t
            org-agenda-skip-scheduled-if-done t
            org-agenda-skip-deadline-prewarning-if-scheduled t
            ;; Searches include archives
            org-agenda-text-search-extra-files '(agenda-archives)
            ;; Ensure the agenda shows the whole coming week.
            org-agenda-start-on-weekday nil
            org-agenda-ndays 7)

      ;; Add GTD agenda views.

      (setq org-agenda-custom-commands
            '(("w" "Weekly Review"
               ((agenda "" ((org-agenda-ndays 7)))
                (stuck "")
                (todo "PROJECT")
                (todo "MAYBE")
                (tags "someday")
                (todo "WAITING")))
              ("g" . "GTD contexts")
              ("ga" "Anywhere" tags-todo "@anywhere")
              ("ge" "Errands"  tags-todo "@errand")
              ("gh" "Home"     tags-todo "@home")
              ("gl" "Leisure"  tags-todo "@leisure")
              ("gw" "Work"     tags-todo "@work")
              ("G" "GTD Block Agenda"
               ((tags-todo "@home")
                (tags-todo "@anywhere")
                (tags-todo "@errand"))
               nil)))

      ;; Email agenda
      ;;
      ;; Commands for a custom agenda task that will email the current day's
      ;; items to `user-mail-address'.

      (defun* cb-org:printable-agenda-string ()
        "Return a formatted string of the agenda suitable for printing or emailing.
Return nil if there are no items to display."
        (let ((date (calendar-gregorian-from-absolute (org-today))))
          (->> (org-agenda-files nil 'ifmode)
            (--map (org-agenda-get-day-entries it date))
            (-flatten)
            (-keep 'substring-no-properties)
            (--map
             (with-temp-buffer
               (insert (s-trim it))
               ;; Remove tags
               (goto-char (point-min))
               (while (search-forward-regexp
                       (rx ":" (* (not space)) ":" (* space) eol)
                       nil t)
                 (replace-match ""))
               (buffer-string)))
            ;; Group by category.
            (--map (cdr (s-match (rx bol (* space)
                                     ;; Match category, excluding dates.
                                     (group
                                      (not (any space digit))
                                      (+ (not space)))
                                     (* space)
                                     ;; Match headline.
                                     (group (+ nonl))
                                     (* space))
                                 it)))
            ;; Group by category. While doing so, titleize the category name.
            ;; Anything in the diary or with a hh:mm timestamp goes under 'Journal'.
            (--group-by (let ((category (->> (car it)
                                          (s-chop-suffix ":")
                                          (s-titleized-words))))
                          (cond
                           ((ignore-errors
                              (s-matches? (rx bol digit digit ":" digit digit) (cdr it)))
                            "Journal")
                           ((s-matches? (rx "diary") category) "Journal")
                           (t
                            category))))
            ;; Chop categories from each item in the grouping.
            ;; Results in a list of [category items ...]
            (--map (cons (car it)
                         (-sort 'string< (-map 'cadr (cdr it)))))
            ;; Create an org-formatted string where the category is the headers and its
            ;; items are an unordered list.
            (--map (destructuring-bind (category &rest items) it
                     (concat "* " category "\n"
                             (s-join "\n" (--map (s-prepend "- " it) items)))))
            ;; Recombine.
            (s-join "\n"))))

      (defun cb-org:mail-agenda (&rest _)
        "Email the current agenda buffer to `user-email-address'."
        (interactive)
        (save-window-excursion
          (let ((agenda (cb-org:printable-agenda-string)))
            (if (s-blank? agenda)
                (message "No agenda items to send")
              (compose-mail user-mail-address
                            (format "[org] Journal for %s"
                                    (calendar-date-string (calendar-current-date) 'abbrev)))
              ;; Insert agenda and apply HTML formatting.
              (insert agenda)
              (let ((org-export-with-toc nil))
                (org-mime-htmlize nil))
              (message-send-and-exit)))))

      (add-to-list 'org-agenda-custom-commands
                   '("@" "Send as email" cb-org:mail-agenda "")
                   'append)

      ;; Agenda sorting

      (setq org-agenda-sorting-strategy
            '((agenda habit-down time-up priority-down category-keep)
              (todo priority-down category-keep scheduled-up)
              (tags priority-down category-keep)
              (search category-keep)))

      ;; Key bindings

      (hook-fn 'org-agenda-mode-hook
        (local-set-key (kbd "g") 'org-agenda-goto-date)
        (local-set-key (kbd "j") 'org-agenda-next-item)
        (local-set-key (kbd "k") 'org-agenda-previous-item))

      (after 'smartparens
        (hook-fn 'org-agenda-mode-hook
          (smartparens-mode -1)))

      ;; Exclude tasks with :hold: tag.

      (defun cb-org:exclude-tasks-on-hold (tag)
        (and (equal tag "hold") (concat "-" tag)))

      (setq org-agenda-auto-exclude-function 'cb-org:exclude-tasks-on-hold)

      ;; Refresh the agenda after saving org buffers.

      (defun cb-org:refresh-agenda ()
        "Refresh all org agenda buffers."
        (save-excursion
          (--each (--filter-buffers (derived-mode-p 'org-agenda-mode))
            (with-current-buffer it
              (ignore-errors
                (org-agenda-redo t))))))

      (hook-fn 'org-mode-hook
        (add-hook 'after-save-hook 'cb-org:refresh-agenda nil 'local))))

  ;; `org-protocol' allows other applications to connect to Emacs and prompt
  ;; org-mode to perform certain actions, including saving links.
  (use-package org-protocol)

  ;; `appt' is emacs' generic scheduling system for calendar. Configure it to
  ;; hook into org-mode.
  (use-package appt
    :config
    (progn
      (setq appt-message-warning-time 60
            appt-display-interval 15)

      (save-window-excursion
        (appt-activate +1))

      ;; Update the appointments ledger when saving the diary file.
      (hook-fn 'org-mode-hook
        (when (equal (buffer-file-name) org-agenda-diary-file)
          (hook-fn 'after-save-hook
            :local t
            (save-window-excursion
              (org-agenda-to-appt t)
              (appt-check 'force)))))))

  ;; `org-clock' provides time-keeping and clocking features for tasks in
  ;; org-mode.
  (use-package org-clock
    :config
    (progn
      (org-clock-persistence-insinuate)
      (setq org-clock-history-length 20
            org-clock-in-resume t
            org-clock-into-drawer t
            org-clock-out-remove-zero-time-clocks t
            org-clock-out-when-done t
            org-clock-persist t
            org-clock-persist-query-resume nil
            org-clock-auto-clock-resolution 'when-no-clock-is-running
            org-clock-report-include-clocking-task t)

      (defvar cb-org:keep-clock-running nil
        "Used to enforce clocking to default task when clocking out.")

      (defun cb-org:project? ()
        "Any task with a todo keyword subtask"
        (save-restriction
          (widen)
          (let ((has-subtask)
                (subtree-end (save-excursion (org-end-of-subtree t)))
                (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
            (save-excursion
              (forward-line 1)
              (while (and (not has-subtask)
                          (< (point) subtree-end)
                          (re-search-forward "^\*+ " subtree-end t))
                (when (member (org-get-todo-state) org-todo-keywords-1)
                  (setq has-subtask t))))
            (and is-a-task has-subtask))))

      (defun cb-org:task? ()
        "Any task with a todo keyword and no subtask"
        (save-restriction
          (widen)
          (let ((has-subtask)
                (subtree-end (save-excursion (org-end-of-subtree t)))
                (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
            (save-excursion
              (forward-line 1)
              (while (and (not has-subtask)
                          (< (point) subtree-end)
                          (re-search-forward "^\*+ " subtree-end t))
                (when (member (org-get-todo-state) org-todo-keywords-1)
                  (setq has-subtask t))))
            (and is-a-task (not has-subtask)))))

      ;; Clocking in changes TODO state to NEXT.

      (defun cb-org:clock-in-to-next-state (_kw)
        "Move a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO."
        (unless (true? org-capture-mode)
          (cond
           ((and (-contains? '("TODO") (org-get-todo-state))
                 (cb-org:task?))
            "NEXT")
           ((and (-contains? '("NEXT") (org-get-todo-state))
                 (cb-org:project?))
            "TODO"))))

      (setq org-clock-in-switch-to-state 'cb-org:clock-in-to-next-state)

      ;; Clocking commands
      ;;
      ;; Provides a pair of commands that, when used religiously, ensures that
      ;; all your time is logged somewhere by org-mode.
      ;;
      ;; The idea is to 'punch in' (C-o C-i) at the start of the workday, then
      ;; 'punch out' (C-o C-o) at the end.  If there's a task at point when you
      ;; punch in, that becomes a default task for time logging; otherwise a
      ;; default task in your notes file is used.  This ensures that all your
      ;; time is being logged somewhere, even if you're not working on a
      ;; specific task.
      ;;
      ;; When you clock out from a task, org-mode will search for an enclosing
      ;; heading to clock in to or fall back to the default task.
      ;;
      ;; See: http://doc.norang.ca/org-mode.html#Clocking

      (defun cb-org:punch-in (arg)
        "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
        (interactive "p")
        (setq cb-org:keep-clock-running t)
        (if (equal major-mode 'org-agenda-mode)
            ;; We're in the agenda
            (let* ((marker (org-get-at-bol 'org-hd-marker))
                   (tags (org-with-point-at marker (org-get-tags-at))))
              (if (and (eq arg 4) tags)
                  (org-agenda-clock-in '(16))
                (cb-org:clock-in-organization-task-as-default)))
          ;; We are not in the agenda
          (save-restriction
            (widen)
            ;; Find the tags on the current task
            (if (and (equal major-mode 'org-mode)
                     (not (org-before-first-heading-p))
                     (eq arg 4))
                (org-clock-in '(16))
              (cb-org:clock-in-organization-task-as-default))))
        (message "Punched in to [%s]." org-clock-current-task))

      (defun cb-org:punch-out ()
        (interactive)
        (setq cb-org:keep-clock-running nil)
        (when (org-clock-is-active)
          (org-clock-out))
        (org-agenda-remove-restriction-lock)
        (message "Punched out."))

      (defun cb-org:clock-in-default-task ()
        (save-excursion
          (org-with-point-at org-clock-default-task
            (org-clock-in))))

      (defun cb-org:clock-in-parent-task ()
        "Move point to the parent (project) task if any and clock in"
        (let ((parent-task))
          (save-excursion
            (save-restriction
              (widen)
              (while (and (not parent-task) (org-up-heading-safe))
                (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                  (setq parent-task (point))))
              (if parent-task
                  (org-with-point-at parent-task
                    (org-clock-in))
                (when cb-org:keep-clock-running
                  (cb-org:clock-in-default-task)))))))

      (defvar cb-org:organization-task-id "EB155A82-92B2-4F25-A3C6-0304591AF2F9")

      (defun cb-org:clock-in-organization-task-as-default ()
        (interactive)
        (org-with-point-at (org-id-find cb-org:organization-task-id 'marker)
          (org-clock-in '(16))))

      (defun cb-org:clock-out-maybe ()
        (when (and cb-org:keep-clock-running
                   (not org-clock-clocking-in)
                   (marker-buffer org-clock-default-task)
                   (not org-clock-resolving-clocks-due-to-idleness))
          (cb-org:clock-in-parent-task)))

      (add-hook 'org-clock-out-hook 'cb-org:clock-out-maybe 'append)

      (bind-keys
        :overriding? t
        "C-o C-c" (command (org-clock-in '(4)))
        "C-o C-i" 'cb-org:punch-in
        "C-o C-o" 'cb-org:punch-out)

      ;; Remove empty LOGBOOK drawers when clocking out.
      (hook-fn 'org-clock-out-hook
        :append t
        (save-excursion
          (beginning-of-line 0)
          (org-remove-empty-drawer-at "LOGBOOK" (point))))

      ;; Automatically change projects from NEXT to TODO

      (defun cb-org:mark-next-parent-tasks-todo ()
        "Visit each parent task and change NEXT states to TODO"
        (let ((mystate (or (and (fboundp 'org-state)
                                state)
                           (nth 2 (org-heading-components)))))
          (when mystate
            (save-excursion
              (while (org-up-heading-safe)
                (when (member (nth 2 (org-heading-components)) (list "NEXT"))
                  (org-todo "TODO")))))))

      (hook-fns '(org-after-todo-state-change-hook org-clock-in-hook)
        :append t
        (cb-org:mark-next-parent-tasks-todo))))

  ;; `org-archive' provides archiving features for org-mode.
  (use-package org-archive
    :config
    (progn

      (defun cb-org:archive-done-tasks ()
        (interactive)
        (atomic-change-group
          (org-map-entries (lambda ()
                             ;; Ensure point does not move past the next item to
                             ;; archive.
                             (setq org-map-continue-from (point))
                             (org-archive-subtree))
                           "/DONE|PAID|VOID|CANCELLED" 'tree)))

      (defalias 'archive-done-tasks 'cb-org:archive-done-tasks)
      (setq org-archive-default-command 'cb-org:archive-done-tasks)

      ;; Apply inherited tags to archived items.
      (defadvice org-archive-subtree
        (before add-inherited-tags-before-org-archive-subtree activate)
        "Add inherited tags before org-archive-subtree."
        (org-set-tags-to (org-get-tags-at)))))

  ;; `org-crypt' provides encryption functions for org buffers.
  (use-package org-crypt
    :config
    (progn
      (org-crypt-use-before-save-magic)
      (setq org-crypt-disable-auto-save nil)
      (add-to-list 'org-tags-exclude-from-inheritance "crypt")

      (define-key org-mode-map (kbd "C-c c") 'org-encrypt-entry)

      ;; Decrypt with C-c C-c

      (defun cb-org:looking-at-pgp-section? ()
        (unless (org-before-first-heading-p)
          (save-excursion
            (org-back-to-heading t)
            (let ((heading-point (point))
                  (heading-was-invisible-p
                   (save-excursion
                     (outline-end-of-heading)
                     (outline-invisible-p))))
              (forward-line)
              (looking-at "-----BEGIN PGP MESSAGE-----")))))

      (hook-fn 'org-ctrl-c-ctrl-c-hook
        (when (cb-org:looking-at-pgp-section?)
          (org-decrypt-entry)
          t))))

  ;; `org-mime' provides MIME exporting functions, allowing you to export org
  ;; buffers to HTML emails.
  (use-package org-mime
    :config
    (progn
      ;; Key bindings

      (hook-fn 'message-mode-hook
        (local-set-key (kbd "C-c M-o") 'org-mime-htmlize))

      (hook-fn 'org-mode-hook
        (local-set-key (kbd "C-c M-o") 'org-mime-org-buffer-htmlize))

      (hook-fn 'org-mime-html-hook
        ;; Offset block quotes and source code.
        (org-mime-change-element-style
         "blockquote" "border-left: 2px solid #B0B0B0; padding-left: 4px;")
        (org-mime-change-element-style
         "pre" "border-left: 2px solid #B0B0B0; padding-left: 4px;")))))

;; Disable auto-complete in org-buffers.
(after 'auto-complete
  (hook-fn 'org-mode-hook
    (setq-local ac-sources nil)))

(provide 'cb-org)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-org.el ends here
