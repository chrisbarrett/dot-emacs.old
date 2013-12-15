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
(require 'cb-osx)
(require 'cb-search)
(autoload 'message-send-and-exit "message")
(autoload 'projectile-project-buffer-names "projectile")
(autoload 'projectile-project-name "projectile")
(autoload 'projectile-project-p "projectile")

;; Declare org-related values before org-mode is loaded.
;; This is mainly to reduce compiler warnings.
(defvar org-directory (f-join user-home-directory "org"))
(defvar org-init-notes-file (f-join org-directory "notes.org")
  "Captures the original value of the `org-default-notes-file'.
`org-default-notes-file' may be changed interactively by
`cb-org:set-notes-file', and is needed for some functions.")
(defvar org-default-notes-file org-init-notes-file)
(defvar org-id-locations-file (f-join cb:tmp-dir "org-id-locations"))
(defvar org-clock-persist-file (f-join user-dropbox-directory ".org-clock-save.el"))
(defvar org-export-publishing-directory (f-join user-home-directory "Desktop"))
(defvar org-agenda-diary-file (f-join org-directory "diary.org"))
(defvar org-export-exclude-tags '("noexport" "crypt"))
(defvar calendar-date-style 'european)

;; Add contrib directory to load-path.
(add-to-list 'load-path (f-join cb:lib-dir "org-mode" "contrib" "lisp"))

;; Add executors and a global picker for common org actions.

(declare-modal-executor org-agenda-fullscreen
  :command (org-agenda-list prefix-arg nil 1))

(declare-modal-executor org-show-todo-list
  :command (progn
             (org-agenda prefix-arg "t")
             (org-agenda-filter-apply '("-someday") 'tag)))

(declare-modal-executor org-tags-view-todos-fullscreen
  :command (org-tags-view t))

(declare-modal-executor org-tags-view-all-fullscreen
  :command (org-tags-view nil))

(declare-modal-executor org-search-view
  :command (call-interactively 'org-search-view))

(defun cb-org:yank-region-as-quote (beg end)
  "Yank the current region as an org quote."
  (interactive "r")
  (if (region-active-p)
      (progn
        (kill-new (cb-org:buffer-substring-to-quote beg end))
        (deactivate-mark)
        (message "Region yanked as quote."))
    (error "No region is active, so no quote could be yanked")))

(defun cb-org:set-notes-file (file)
  "Select the notes file to use as the default.
This will set which file org-capture will capture to."
  (interactive
   (list
    (let* ((fs (org-files-list))
           (selected
            (ido-completing-read
             "File: "
             (->> fs
               (-remove (C (~ equal "org_archive") f-ext))
               (-map 'f-filename)))))
      (-first (C (~ equal selected) f-filename) fs))))
  (setq org-default-notes-file file))

(defun cb-org:find-diary ()
  (interactive)
  (find-file org-agenda-diary-file))

(defun cb-org:find-notes ()
  (interactive)
  (find-file org-default-notes-file))

(defun cb-org:capture ()
  "Adapt `org-capture' to my own selection widget."
  (interactive)
  (let ((k (car (read-option "*Org Capture*" 'car 'cadr
                             org-capture-templates))))
    (org-capture nil k)))

(define-command-picker org-action-picker
  :title "*Org Commands*"
  :options
  `(("a" "Agenda" org-agenda)
    ("b" "Buffers" org-iswitchb)
    ("c" "Follow Clock" org-clock-goto)
    ("d" "Go to Diary" cb-org:find-diary)
    ("f" "Set Notes File" cb-org:set-notes-file)
    ("g" "Go to Subtree" ,(command (org-refile 'goto)))
    ("k" "Capture" cb-org:capture)
    ("l" "Store Link" org-store-link)
    ("n" "Go to Notes" cb-org:find-notes)
    ("s" "Search" executor:org-search-view)
    ("t" "Todo List" executor:org-show-todo-list)
    ("v" "View Tags (todos)" executor:org-tags-view-todos-fullscreen)
    ("V" "View Tags (all)" executor:org-tags-view-all-fullscreen)
    ("y" "Yank Region as Quote" cb-org:yank-region-as-quote :when region-active-p)))

(bind-keys
  :overriding? t
  "C-c a" 'org-agenda
  "C-c l" 'org-store-link
  "<f6>" (command (org-capture nil "t"))
  "<f7>" 'cb-org:capture
  "<f8>" 'org-action-picker
  "<f9>" 'executor:org-agenda-fullscreen)

;; If we're running in a graphical context, show the agenda on startup.
(when (or (daemonp) (display-graphic-p))
  (hook-fn 'after-init-hook
    (unless noninteractive
      (executor:org-agenda-fullscreen))))

;; Add `org-search-view' to search methods picker.
(cbs-define-search-method
 :name "Org Files"
 :key "o"
 :command (lambda (_)
            (call-interactively 'org-search-view)))

;; `org-mode' is a suite of editing and management tools centred around
;; human-readable text files.
(use-package org
  :if (not noninteractive)
  :defer t
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
     org-startup-with-inline-images t
     org-edit-src-content-indentation 0
     org-catch-invisible-edits 'smart
     org-insert-heading-respect-content nil
     org-M-RET-may-split-line nil

     ;; Org habit

     org-habit-preceding-days 14
     org-habit-following-days 4
     org-habit-graph-column 70

     ;; Exporter options

     org-html-html5-fancy t
     org-export-backends '(ascii html latex md)

     ;; Statistics
     ;;
     ;; Perhaps confusingly, setting hierarchy vars to non-nil values makes
     ;; statistics functions shallow.

     org-hierarchical-todo-statistics nil
     org-checkbox-hierarchical-statistics t

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

     ;; Use lowercase structure keys, e.g. "begin_src" instead of "BEGIN_SRC"
     org-structure-template-alist
     (-map (~ -map 's-downcase) org-structure-template-alist)

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
       ("@computer" . ?c)
       ("@errand" . ?e)
       ("@home" . ?h)
       ("@leisure" . ?l)
       ("@phone" . ?p)
       ("@work" . ?w)
       (:endgroup . nil)))

    ;; Set some default effort times.
    (add-to-list 'org-global-properties
                 `("Effort_ALL" .
                   ,(concat "1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00 9:00 "
                            "0:05 0:10 0:30")))

    (defun cb-org:ctrl-c-ctrl-k (&optional n)
      "Kill subtrees, unless we're in a special buffer where it should cancel."
      (interactive "p")
      (if (s-starts-with? "*Org" (buffer-name))
          (org-kill-note-or-show-branches)
        (org-cut-subtree n)))

    (defun cb-org:ctrl-c-ret ()
      "Call `org-table-hline-and-move' or `org-insert-todo-heading' dep. on context."
      (interactive)
      (cond
       ((org-at-table-p) (call-interactively 'org-table-hline-and-move))
       (t (call-interactively 'org-insert-todo-heading))))

    (define-keys org-mode-map
      "C-c C-k" 'cb-org:ctrl-c-ctrl-k
      "C-c C-." 'org-time-stamp-inactive
      "C-c o" 'org-attach-open
      "M-p" 'org-metaup
      "M-n" 'org-metadown
      "C-c c" 'org-columns
      "C-c RET" 'cb-org:ctrl-c-ret
      "C-c C-j" (command (org-refile 'goto))
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
      (save-restriction
        (org-table-map-tables 'org-table-align 'quiet)
        ;; Realign tags.
        (org-set-tags 4 t)
        ;; Remove empty properties drawers.
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward ":PROPERTIES:" nil t)
            (save-excursion
              (org-remove-empty-drawer-at "PROPERTIES" (match-beginning 0)))))))

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
                                 (ledger . t)
                                 (C . t)
                                 (ditaa . t)
                                 (sh . t)
                                 (calc . t)
                                 (scala . t)
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

;; `orglink' adds support for org-mode-style and plain links in other modes.
(use-package orglink
  :if (not noninteractive)
  :ensure t
  :defer t
  :init
  (hook-fns '(prog-mode-hook text-mode-hook comint-mode)
    (ignore-errors
      (unless (derived-mode-p 'org-mode 'nxml-mode 'sgml-mode 'snippet-mode)
        (orglink-mode +1))))
  :config
  (progn
    (setq orglink-mode-lighter nil
          orglink-activate-links '(angle plain))))

;; `org-pomodoro' adds Pomodoro clocking functions.
;; I have my own fork, since the original isn't keeping up with pull requests.
(use-package org-pomodoro
  :if (not noninteractive)
  :defer t
  :bind ("<f5>" . org-pomodoro)
  :config
  (progn

    (setq org-pomodoro-format "• %s"
          org-pomodoro-short-break-format "Break %s"
          org-pomodoro-long-break-format "Break %s"
          org-pomodoro-long-break-length 25
          org-pomodoro-show-seconds nil
          ;; The modeline timer is managed in `cb-modeline'.
          org-pomodoro-show-in-mode-line nil)

    ;; Notifications

    (defun cb-org:pomodoro-growl ()
      (growl "Pomodoro"
             (cl-case org-pomodoro-state
               (:pomodoro (format "Timer started (%s/%s)"
                                  (1+ (mod org-pomodoro-count
                                           org-pomodoro-long-break-frequency))
                                  org-pomodoro-long-break-frequency))
               (:short-break "Short break")
               (:long-break  "Long break")
               (otherwise "Stopped"))
             (f-join cb:assets-dir "org-pomodoro.png")))

    (defun cb-org:pomodoro-growl-end-break ()
      (growl "Pomodoro"
             "Break finished"
             (f-join cb:assets-dir "org-pomodoro.png")))

    (when (equal system-type 'darwin)
      ;; Use system sounds for alerts.
      (let ((snd (osx-find-system-sound "purr")))
        (setq org-pomodoro-sound snd
              org-pomodoro-short-break-sound snd
              org-pomodoro-long-break-sound snd))

      ;; Show growl notifications
      (add-hook 'org-pomodoro-finished-hook 'cb-org:pomodoro-growl)
      (add-hook 'org-pomodoro-started-hook 'cb-org:pomodoro-growl)
      (add-hook 'org-pomodoro-killed-hook 'cb-org:pomodoro-growl)
      (add-hook 'org-pomodoro-short-break-finished-hook 'cb-org:pomodoro-growl-end-break)
      (add-hook 'org-pomodoro-short-break-finished-hook 'cb-org:pomodoro-growl-end-break))))

;;; Configure org-mode sub-features only if org-mode is actually loaded.

;; Use `iimage' to display inline images in org buffers.
(after 'org
  (use-package iimage
    :config
    (progn
      (add-to-list 'iimage-mode-image-regex-alist
                   (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
                                 "\\)\\]")  1))

      (defun org-toggle-iimage-in-org ()
        "Display images in the current orgmode buffer."
        (interactive)
        (if (face-underline-p 'org-link)
            (set-face-underline-p 'org-link nil)
          (set-face-underline-p 'org-link t))
        (iimage-mode)))))

(add-hook 'org-mode-hook 'auto-fill-mode)

;; `org-capture' is used to interactively read items to be inserted into
;; org-mode buffers.
(after 'org
  (use-package org-capture
    :config
    (progn

      (add-hook 'org-capture-before-finalize-hook 'indent-buffer 'append)

      (-each `(("t" "Todo" entry
                (file+headline org-default-notes-file "Tasks")
                ,(s-unlines
                  "* TODO %?                               :@computer:"
                  "SCHEDULED: %t"
                  ":PROPERTIES:"
                  ":CAPTURED: %U"
                  ":END:")
                :clock-keep t)

               ("s" "Someday" entry
                (file+headline org-default-notes-file "Someday")
                ,(s-unlines
                  "* MAYBE %?                              :@computer:"
                  ":PROPERTIES:"
                  ":CAPTURED: %U"
                  ":END:")
                :clock-keep t)

               ("d" "Diary" entry
                (file+datetree org-agenda-diary-file)
                "* %?\n%^t"
                :clock-keep t)

               ("h" "Habit" entry
                (file+headline org-default-notes-file "Habits/Recurring")
                ,(s-unlines
                  "* TODO %?"
                  "SCHEDULED: %t"
                  ":PROPERTIES:"
                  ":STYLE: habit"
                  ":END:"
                  ":PROPERTIES:"
                  ":CAPTURED: %U"
                  ":END:")
                :clock-keep t)

               ("p" "Phone Call (in)" entry
                (file+headline org-default-notes-file "Calls")
                ,(s-unlines
                  "* %U"
                  "- From :: %?"
                  "- To :: "
                  "- Subject :: ")
                :clock-in t
                :clock-resume t)

               ("o" "Phone Call (out)" entry
                (file+headline org-default-notes-file "Calls")
                ,(s-unlines
                  "* %U"
                  "- To :: %^{To}"
                  "- Subject :: %^{Subject}"
                  "%?")
                :clock-in t
                :clock-resume t)

               ("b" "Bill" entry
                (file+olp org-default-notes-file "Finance" "Bills")
                ,(s-unlines
                  "* TODO %?"
                  ":PROPERTIES:"
                  ":CAPTURED: %U"
                  ":END:")
                :clock-keep t)

               ("l" "Link" entry
                (file+headline org-default-notes-file "Links")
                ,(s-unlines
                  "* %c"
                  ":PROPERTIES:"
                  ":CAPTURED: %U"
                  ":END:"
                  "%i")
                :immediate-finish t
                :clock-keep t)

               ("m" "Listening" entry
                (file+olp org-default-notes-file "Someday" "Listening")
                ,(s-unlines
                  "* MAYBE Listen to %i%?"
                  ":PROPERTIES:"
                  ":CAPTURED: %U"
                  ":END:")
                :clock-keep t)

               ("v" "Viewing" entry
                (file+olp org-default-notes-file "Someday" "Viewing")
                ,(s-unlines
                  "* MAYBE Watch %i%?"
                  ":PROPERTIES:"
                  ":CAPTURED: %U"
                  ":END:")
                :clock-keep t)

               ("r" "Reading" entry
                (file+olp org-default-notes-file "Someday" "Reading")
                ,(s-unlines
                  "* MAYBE Read %i%?"
                  ":PROPERTIES:"
                  ":CAPTURED: %U"
                  ":END:")
                :clock-keep t)

               ("n" "Note" entry
                (file+headline org-default-notes-file "Notes")
                ,(s-unlines
                  "* %i%?"
                  ":PROPERTIES:"
                  ":CAPTURED: %U"
                  ":END:")
                :clock-keep t)

               ("z" "Task Note" entry
                (clock)
                ,(s-unlines
                  "* %i%?"
                  ":PROPERTIES:"
                  ":CAPTURED: %U"
                  ":END:")
                :clock-keep t
                :kill-buffer t)

               ("L" "Task Link" entry
                (clock)
                ,(s-unlines
                  "* %a%?"
                  ":PROPERTIES:"
                  ":CAPTURED: %U"
                  ":END:")
                :clock-keep t
                :kill-buffer t)
               )
             (~ add-to-list 'org-capture-templates)))))

;; `org-agenda' provides an interactive journaling system, collecting
;; information from org-mode buffers.
(after 'org
  (use-package org-agenda
    :config
    (progn

      ;; Define a hook for setting up agenda windows.

      (defvar org-agenda-customise-window-hook nil
        "Relay hook for `org-agenda-mode-hook'. Suitable for setting up the window.")
      (hook-fn 'org-agenda-mode-hook
        (run-hooks 'org-agenda-customise-window-hook))

      ;; Show today's agenda after a period of inactivity.

      (defvar cb-org:show-agenda-idle-delay (* 30 60)
        "The delay in seconds after which to pop up today's agenda.")

      (defvar cb-org:show-agenda-idle-timer
        (unless noninteractive
          (run-with-idle-timer cb-org:show-agenda-idle-delay t
                               'executor:org-agenda-fullscreen))
        "Idle timer that will display today's org agenda.
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
            ;; Searches are boolean, like a Google search.
            org-agenda-search-view-always-boolean t
            ;; Ensure the agenda shows the whole coming week.
            org-agenda-start-on-weekday nil
            org-agenda-ndays 7)

      ;; Add GTD agenda views. They should be displayed modally.

      (setq org-agenda-custom-commands
            (->> '(("n" "Next actions"
                    ((tags-todo "-someday/NEXT")))
                   ("w" "Weekly Review"
                    ((agenda "" ((org-agenda-ndays 7)))
                     (stuck "")
                     (todo "WAITING")
                     (tags-todo "someday-media-skill")
                     (tags-todo "someday&skill")
                     (tags-todo "someday&media")))
                   ("g" . "GTD contexts")
                   ("gg" "Anywhere"
                    ((tags-todo "@computer")
                     (tags-todo "@errand")
                     (tags-todo "@home")
                     (tags-todo "@leisure")
                     (tags-todo "@phone")
                     (tags-todo "@work")))
                   ("ge" "Errands"  tags-todo "@errand")
                   ("gp" "Phone"    tags-todo "@phone")
                   ("gw" "Work"     tags-todo "@work")
                   ("gh" "Home"     tags-todo "@home")
                   ("gl" "Leisure"  tags-todo "@leisure"))
              (--map-when (listp (cdr it))
                          (append it
                                  '(((org-agenda-customise-window-hook
                                      'delete-other-windows)))))))

      ;; Email agenda
      ;;
      ;; Commands for a custom agenda task that will email the current day's
      ;; items to `user-mail-address'.

      (defun cb-org:printable-agenda-string ()
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
            (--map (cl-destructuring-bind (category &rest items) it
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

      (after 'org-agenda
        (bind-keys
          :map org-agenda-mode-map
          "C" 'org-agenda-capture
          "g" 'org-agenda-goto-date
          "j" 'org-agenda-next-item
          "k" 'org-agenda-previous-item))

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
        (save-window-excursion
          (let ((inhibit-redisplay t))
            (save-excursion
              (--each (--filter-buffers (derived-mode-p 'org-agenda-mode))
                (with-current-buffer it
                  (ignore-errors
                    (org-agenda-redo t))))))))

      (hook-fn 'org-mode-hook
        (add-hook 'after-save-hook 'cb-org:refresh-agenda nil 'local)))))

;; Define commands for working with diary entries.
(after 'org-agenda

  (defun diary-limited-cyclic (recurrences interval m d y)
    "For use in emacs diary. Cyclic item with limited number of recurrences.
Occurs every INTERVAL days, starting on YYYY-MM-DD, for a total of
RECURRENCES occasions."
    (let ((startdate (calendar-absolute-from-gregorian (list m d y)))
          (today (calendar-absolute-from-gregorian date)))
      (and (not (cl-minusp (- today startdate)))
           (zerop (% (- today startdate) interval))
           (< (floor (- today startdate) interval) recurrences))))

  (defun org-insert-class ()
    "Read and insert a class diary sexp at point."
    (interactive "*")
    (insert (org-read-class)))

  (cl-defun cb-org:format-class-sexpr ((s1 m1 h1 d1 m1 y1 . _)
                                       (s2 m2 h2 d2 m2 y2 . _)
                                       desc)
    "Parse dates into an org-class s-expression."
    (let* ((time (unless (or (zerop m1) (zerop h1)) (format " %.2i:%.2i %s" h1 m1 desc)))
           (date-range (list (list y1 m1 d1) (list y2 m2 d2)))
           (date-cols (-map (C
                             (~ s-pad-right 12 " ")
                             (~ s-join " ")
                             (~ -map (C (~ s-pad-left 2 " ")
                                        'number-to-string)))
                            date-range))
           (day-of-week (number-to-string (calendar-day-of-week (list m1 d1 y1)))))
      (concat "<%%(org-class   "
              (s-join " "  (-concat date-cols (list day-of-week)))
              ")>" time)))

  (defun org-read-class ()
    "Read a class diary sexp with a description.
The starting day is taken to be the weekday on which the event will repeat."
    (let ((desc (read-string "Description: ")))
      (cb-org:format-class-sexpr
       (org-parse-time-string (org-read-date nil nil nil "Start date: "))
       (org-parse-time-string (org-read-date nil nil nil "End date: "))
       desc))))

;; `org-protocol' allows other applications to connect to Emacs and prompt
;; org-mode to perform certain actions, including saving links.
(after 'org
  (use-package org-protocol))

;; `appt' is emacs' generic scheduling system for calendar. Configure it to
;; hook into org-mode.
(after 'org
  (use-package appt
    :config
    (progn
      (setq appt-message-warning-time 60
            appt-display-interval 15)

      (save-window-excursion
        (appt-activate +1))

      (defun cb-org:save-diary ()
        (save-restriction
          (save-window-excursion
            (org-agenda-to-appt t)
            (appt-check 'force))))

      ;; Update the appointments ledger when saving the diary file.
      (hook-fn 'org-mode-hook
        (when (equal (buffer-file-name) org-agenda-diary-file)
          (add-hook 'after-save-hook 'cb-org:save-diary nil t))))))

;; `org-clock' provides time-keeping and clocking features for tasks in
;; org-mode.
(after 'org
  (use-package org-clock
    :if (not noninteractive)
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

      ;; Remove empty LOGBOOK drawers when clocking out.
      (hook-fn 'org-clock-out-hook
        :append t
        (save-excursion
          (beginning-of-line 0)
          (org-remove-empty-drawer-at "LOGBOOK" (point))))

      ;; Automatically change projects from NEXT to TODO

      (defun cb-org:mark-next-parent-tasks-todo ()
        "Visit each parent task and change state to TODO"
        (let ((mystate (or (and (fboundp 'org-state)
                                state)
                           (nth 2 (org-heading-components)))))
          (when mystate
            (save-excursion
              (while (org-up-heading-safe)
                (when (-contains? '("NEXT" "WAITING" "MAYBE")
                                  (nth 2 (org-heading-components)))
                  (org-todo "TODO")))))))

      (hook-fns '(org-after-todo-state-change-hook org-clock-in-hook)
        :append t
        (cb-org:mark-next-parent-tasks-todo)))))

;; `org-archive' provides archiving features for org-mode.
(after 'org
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
        (org-set-tags-to (org-get-tags-at))))))

;; `org-crypt' provides encryption functions for org buffers.
(after 'org
  (use-package org-crypt
    :config
    (progn
      (org-crypt-use-before-save-magic)
      (setq org-crypt-disable-auto-save 'encypt)
      (add-to-list 'org-tags-exclude-from-inheritance "crypt")

      (define-key org-mode-map (kbd "C-c x") 'org-encrypt-entry)

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

      (defun cb-org:decrypt-entry ()
        (when (cb-org:looking-at-pgp-section?)
          (org-decrypt-entry)
          t))

      (add-hook 'org-ctrl-c-ctrl-c-hook 'cb-org:decrypt-entry))))

;; `org-attach' adds support for attachments to subtrees as an alternative to
;; plain links.
(after 'org
  (use-package org-attach
    :config
    (progn
      (setq org-link-abbrev-alist '(("att" . org-attach-expand-link))
            org-attach-directory (f-join org-directory "data"))

      ;; HACK: Redefine `org-attach-attach' to use helm to read files.
      (after 'org-attach
        (defun org-attach-attach (file &optional visit-dir method)
          "Move/copy/link FILE into the attachment directory of the current task.
If VISIT-DIR is non-nil, visit the directory with dired.
METHOD may be `cp', `mv', `ln', or `lns' default taken from
`org-attach-method'."
          (interactive
           (list
            (helm-read-file-name "File to keep as an attachment: ")
            current-prefix-arg))
          (setq method (or method org-attach-method))
          (let ((basename (file-name-nondirectory file)))
            (when (and org-attach-file-list-property (not org-attach-inherited))
              (org-entry-add-to-multivalued-property
               (point) org-attach-file-list-property basename))
            (let* ((attach-dir (org-attach-dir t))
                   (fname (expand-file-name basename attach-dir)))
              (cond
               ((eq method 'mv)	(rename-file file fname))
               ((eq method 'cp)	(copy-file file fname))
               ((eq method 'ln) (add-name-to-file file fname))
               ((eq method 'lns) (make-symbolic-link file fname)))
              (org-attach-commit)
              (org-attach-tag)
              (cond ((eq org-attach-store-link-p 'attached)
                     (org-attach-store-link fname))
                    ((eq org-attach-store-link-p t)
                     (org-attach-store-link file)))
              (if visit-dir
                  (dired attach-dir)
                (message "File \"%s\" is now a task attachment." basename)))))))))

;; `org-mime' provides MIME exporting functions, allowing you to export org
;; buffers to HTML emails.
(after 'org
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

;; `ox-koma-letter' adds TeX formal letter exporting using KOMA.
(after 'org
  (use-package ox-koma-letter
    :config
    (add-to-list 'org-latex-classes '("koma-letter" "
\\documentclass[paper=A4,pagesize,fromalign=right,
               fromrule=aftername,fromphone,fromemail,
               version=last]{scrlttr2}
\\usepackage[english]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage[normalem]{ulem}
\\usepackage{booktabs}
\\usepackage{graphicx}
[NO-DEFAULT-PACKAGES]
[EXTRA]
[PACKAGES]"))))

;; `org-src' provides source editing functions.
(after 'org
  (use-package org-src
    :config
    (progn

      (defvar org-edit-src-before-exit-hook nil
        "Hook run before exiting a code block.")
      (defadvice org-edit-src-exit (before run-hook activate)
        (run-hooks 'org-edit-src-before-exit-hook))

      ;; Do not add a final newline to org source buffers.
      (hook-fn 'org-src-mode-hook
        (setq-local require-final-newline nil))
      ;; Remove trailing spaces when exiting org code blocks.
      (add-hook 'org-edit-src-before-exit-hook 'delete-trailing-whitespace))))

;; `org-reveal' provides a Reveal.js exporter for making presentations.
(after 'org
  (use-package ox-reveal
    :ensure t
    :config
    (progn
      (setq org-reveal-root (concat "file://" (f-join cb:lib-dir "reveal.js")))

      ;; Auxiliary functions used by snippets.

      (defun cb-org:reveal-read-transition ()
        (popup-menu*
         (-map 'popup-make-item
               '("Cube" "Page" "Concave" "Zoom" "Linear" "Fade" "None" "Default"))
         :isearch t))

      (defun cb-org:reveal-read-theme ()
        (popup-menu*
         (-map 'popup-make-item
               '("Default" "Sky" "Beige" "Simple" "Serif" "Night Moon" "Simple" "Solarized"))
         :isearch t))

      (defun cb-org:reveal-read-frag-style ()
        (popup-menu*
         (-map 'popup-make-item
               '("grow" "shrink" "roll-in" "fade-out"
                 "highlight-red" "highlight-green" "highlight-blue"))
         :isearch t)))))

;; Define pairs for org-mode blocks.
(after 'smartparens
  (sp-with-modes '(org-mode)
    (sp-local-pair "#+BEGIN_SRC" "#+END_SRC")
    (sp-local-pair "#+begin_src" "#+end_src")
    (sp-local-pair "#+BEGIN_EXAMPLE" "#+END_EXAMPLE")
    (sp-local-pair "#+begin_example" "#+end_example")
    (sp-local-pair "#+BEGIN_VERSE" "#+END_VERSE")
    (sp-local-pair "#+begin_verse" "#+end_verse")))

;; Disable auto-complete in org-mode buffers.
(after 'auto-complete
  (hook-fn 'org-mode-hook
    (setq-local ac-sources nil)
    (auto-complete-mode -1)))

;; Configure evil-mode key bindings.
(after 'evil

  (defadvice org-toggle-heading (after goto-line-end activate)
    "Prevent point from being moved to the line beginning."
    (when (s-matches? (rx bol (+ "*") (* space) eol) (current-line))
      (goto-char (line-end-position))))

  (defadvice org-return (around newlines-only-in-insert-state activate)
    "Only insert newlines if we're in insert state."
    (noflet ((newline (&rest args)
                      (when (evil-insert-state-p)
                        (funcall this-fn args))))
      ad-do-it))

  (defadvice org-insert-heading (after insert-state activate)
    "Enter evil insert state"
    (when (fboundp 'evil-insert-state)
      (evil-insert-state nil)))

  (defadvice org-insert-todo-heading (after insert-state activate)
    "Enter evil insert state"
    (when (fboundp 'evil-insert-state)
      (evil-insert-state nil)))

  (defun cborg-evil-fold ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (org-global-cycle 1)
      (recenter)))

  (defun cborg-evil-reveal ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (org-global-cycle 16)
      (recenter)))

  (evil-define-key 'normal org-mode-map
    (kbd "<return>") 'org-return
    (kbd "M-P") 'outline-previous-visible-heading
    (kbd "M-N") 'outline-next-visible-heading
    (kbd "SPC") 'org-cycle
    (kbd "z m") 'cborg-evil-fold
    (kbd "z r") 'cborg-evil-reveal)

  (after 'org-agenda
    (bind-keys
      :map org-agenda-mode-map
      "L"   'org-agenda-log-mode
      "l"   'evil-forward-char
      "h"   'evil-backward-char
      "C-f" 'evil-scroll-page-down
      "C-b" 'evil-scroll-page-up))

  ;; Enter insertion mode in capture buffer.
  (hook-fn 'org-capture-mode-hook
    (evil-insert-state)))


;; Prevent flyspell from incorrectly flagging common org-mode content such as
;; code blocks and encrypted regions.

(defun cb-org:in-no-spellcheck-zone? ()
    (thing-at-point-looking-at (rx "#+begin_nospell" (*? anything ) "#+end_nospell")))

;; Define a skeleton for non-spell-checked regions.
(after 'org
  (add-to-list 'org-structure-template-alist '("n" "#+begin_nospell\n?\n#+end_nospell" "?")))

(defun cb-org:flyspell-verify ()
  "Prevent common flyspell false positives in org-mode."
  (and (ignore-errors
         (org-mode-flyspell-verify))
       (not (or
             (ignore-errors (org-at-encrypted-entry-p))
             (ignore-errors (org-in-src-block-p))
             (ignore-errors (org-at-TBLFM-p))
             (ignore-errors (org-in-block-p '("src" "example" "latex" "html")))
             (ignore-errors (org-in-verbatim-emphasis))
             (ignore-errors (org-in-drawer-p))
             (thing-at-point-looking-at (rx bol "#+" (* nonl) eol))
             (cb-org:in-no-spellcheck-zone?)))))

(put 'org-mode 'flyspell-mode-predicate 'cb-org:flyspell-verify)
(hook-fn 'org-mode-hook
  (setq-local flyspell-generic-check-word-predicate 'cb-org:flyspell-verify))

(provide 'cb-org)

;; Local Variables:
;; End:

;;; cb-org.el ends here
