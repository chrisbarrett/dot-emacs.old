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

;; Configuration for org

;;; Code:

(require 'use-package)
(require 'cb-foundation)
(require 'cb-mode-groups)
(require 'noflet)
(require 'cb-lib)
(require 'cb-evil)
(autoload 'projectile-project-p "projectile")
(autoload 'projectile-project-name "projectile")
(autoload 'projectile-project-buffer-names "projectile")

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

(declare-modal-executor org-agenda-fullscreen
  :bind "M-O"
  :command (org-agenda-list prefix-arg nil 1))

(when (or (daemonp) (display-graphic-p))
  (hook-fn 'after-init-hook
    (executor:org-agenda-fullscreen)))

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
    (cb-org:ensure-field (format "#+FILETAGS: :%s:" (s-alnum-only project-name)))))

(defun cb-org:skip-headers ()
  "Move point past the header lines of an org document."
  (while (and (or (emr-line-matches? (rx bol (or "#+" "-*-"))) (emr-blank-line?))
              (not (save-excursion (forward-line) (eobp))))
    (forward-line)))

(after 'smartparens
  (sp-with-modes '(org-mode)
    (sp-local-pair "#+BEGIN_SRC" "#+END_SRC")
    (sp-local-pair "#+begin_src" "#+end_src")))

(after 'message
  (add-hook 'message-mode-hook 'orgstruct++-mode)
  (add-hook 'message-mode-hook 'orgtbl-mode)
  (define-key message-mode-map (kbd "C-c RET RET") 'org-ctrl-c-ret))

(use-package org
  :defer t
  :init
  (progn
    ;; Override the default M-o bindings with org commands.
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
      :bind "C-o t"
      :command (org-agenda prefix-arg "t"))

    (declare-modal-executor org-show-filtered-todo-list
      :bind "C-o T"
      :command (org-agenda prefix-arg "T"))

    (declare-modal-executor org-tags-view-fullscreen
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
       ("@project" . ?p)
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
      "z m" (command (org-global-cycle 1))
      "z r" (command (org-global-cycle 0)))

    (define-keys org-mode-map
      "C-c C-." 'org-time-stamp-inactive
      ;; disable annoying comment toggle key
      "C-c ;" nil)

;;;; Hooks

    ;; Enter insert state for popup notes.
    (hook-fn 'org-mode-hook
      (when (and (equal (buffer-name) "*Org Note*"))
        (cb:append-buffer)))

    ;; Diminish org minor modes.
    (hook-fn 'cb:org-minor-modes-hook
      (--each cb:org-minor-modes
        (ignore-errors (diminish it))))

    ;; Tidy org buffer before save.
    (hook-fn 'org-mode-hook
      (hook-fn 'before-save-hook
        :local t
        (org-table-map-tables 'org-table-align 'quiet)
        ;; Realign tags.
        (org-set-tags 4 t)))

    ;; Sub-task completion triggers parent completion.
    (hook-fn 'org-after-todo-statistics-hook
      :arglist (n-done n-not-done)
      (let (org-log-done org-log-states) ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;;;; Auto-save notes file

    (defvar cb-org:notes-save-idle-delay 40)

    (defvar cb-org:notes-save-timer
      (run-with-idle-timer cb-org:notes-save-idle-delay t 'cb-org:save-notes)
      "Timer that automatically saves the notes buffer on idle.")

    (defun cb-org:save-notes ()
      "Save the notes file."
      (-when-let (buf (--first-buffer (equal (buffer-file-name it)
                                             org-default-notes-file)))
        (with-current-buffer buf
          (when (buffer-modified-p)
            (save-buffer)))))

;;;; Org babel

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

;;;; Org config

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
             "TODO(t)" "NEXT(n!)" "OUTSTANDING(o)" "WAITING(w@/!)" "APPT(a!)"
             "|"
             "DONE(d!)" "PAID(p!)" "VOID(v@)" "CANCELLED(c@)" "DELEGATED(D!)"))

          org-todo-keyword-faces
          '(("NEXT" . org-todo-next)))

    (--each '("NOTES" "COMMENTS" "PROPERTIES" "LOGBOOK")
      (add-to-list 'org-drawers it))

    (add-hook 'org-mode-hook 'auto-revert-mode)

    (define-keys org-mode-map
      "M-p" 'org-metaup
      "M-n" 'org-metadown)

    (defadvice org-add-log-note (before exit-minibuffer activate)
      "Prevent attempts to expand the minibuffer."
      (when (minibufferp (window-buffer (selected-window)))
        (other-window 1)))))

(after 'org

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
                         ":%^{Context|@anywhere|@errand|@funtimes|@home|@project|@work}:")
                 "SCHEDULED: %^{Schedule}t"
                 ":LOGBOOK:"
                 ":CAPTURED: %U"
                 ":END:")
               :clock-in t
               :clock-resume t)

              ("s" "Todo Someday" entry
               (file+olp org-default-notes-file "Someday" "Tasks")
               ,(s-unlines
                 "* TODO %^{Description}%?"
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
                         ":%^{Context|@anywhere|@errand|@funtimes|@home|@project|@work}:")
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


  (use-package org-agenda
    :config
    (progn

      (add-hook 'org-agenda-mode-hook 'org-agenda-to-appt)
      (add-to-list 'org-agenda-files org-directory)

      ;; Add GTD agenda views.
      (--each '(("r" "Weekly Review"
                 ((agenda "" ((org-agenda-ndays 7)))
                  (stuck "")
                  (todo "PROJECT")
                  (todo "MAYBE")
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
                 ;; no local settings
                 nil)
                )
        (add-to-list 'org-agenda-custom-commands it))

      (setq org-agenda-insert-diary-extract-time t
            org-agenda-span 'week
            org-agenda-skip-deadline-if-done t
            org-agenda-skip-scheduled-if-done t
            org-agenda-skip-deadline-prewarning-if-scheduled t
            ;; For all you night owls.
            org-extend-today-until 4
            ;; Searches include archives
            org-agenda-text-search-extra-files '(agenda-archives)
            ;; Ensure the agenda shows the whole coming week.
            org-agenda-start-on-weekday nil
            org-agenda-ndays 7)

    ;;;; Keys

      (define-keys org-agenda-mode-map
        "g" 'org-agenda-goto-date
        "j" 'org-agenda-next-item
        "k" 'org-agenda-previous-item)

      (after 'smartparens
        (hook-fn 'org-agenda-mode-hook
          (smartparens-mode -1)))

    ;;;; Exclude tasks with HOLD state

      (defun cb-org:exclude-tasks-on-hold (tag)
        (and (equal tag "hold") (concat "-" tag)))

      (setq org-agenda-auto-exclude-function 'cb-org:exclude-tasks-on-hold)

    ;;;; Agenda refresh

      (defun cb-org:refresh-agenda ()
        "Refresh all org agenda buffers."
        (save-excursion
          (--each (--filter-buffers (derived-mode-p 'org-agenda-mode))
            (with-current-buffer it
              (ignore-errors
                (org-agenda-redo t))))))

      (hook-fn 'org-mode-hook
        (add-hook 'after-save-hook 'cb-org:refresh-agenda nil 'local))))


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

    ;;; Clocking in changes TODO state to NEXT.

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

    ;;; Clocking commands

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

    ;;; Automatically change projects from NEXT to TODO

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
      (setq org-archive-default-command 'cb-org:archive-done-tasks)))

  (use-package org-crypt
    :config
    (progn
      (org-crypt-use-before-save-magic)
      (setq org-crypt-disable-auto-save nil)
      (add-to-list 'org-tags-exclude-from-inheritance "crypt")

      (define-key org-mode-map (kbd "C-c c") 'org-encrypt-entry)

    ;;;; Decrypt with C-c C-c

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
          t)))))

(after 'auto-complete
  (hook-fn 'org-mode-hook
    (setq-local ac-sources nil)))

(provide 'cb-org)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-org.el ends here
