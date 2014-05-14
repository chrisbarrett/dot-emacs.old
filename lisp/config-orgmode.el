;;; config-orgmode.el --- configure org-mode

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

;; Configure org-mode

;;; Code:

(require 'utils-common)
(require 'utils-ui)

(defvar org-init-notes-file org-default-notes-file
  "Captures the original value of the `org-default-notes-file'.")

(setq org-clock-persist-file  (f-join org-directory ".org-clock-save"))

(setq org-id-locations-file   (f-join cb:tmp-dir "org-id-locations")
      org-agenda-diary-file   (f-join org-directory "diary.org")
      calendar-date-style     'european)

(setq org-completion-use-ido t
      org-link-mailto-program (quote (compose-mail "%a" "%s"))
      org-insert-heading-respect-content nil
      org-M-RET-may-split-line nil
      org-blank-before-new-entry nil
      org-catch-invisible-edits 'smart
      org-return-follows-link t
      org-support-shift-select t)

(setq org-footnote-auto-adjust t)

(add-hook 'org-mode-hook 'auto-revert-mode)

(add-hook 'org-mode-hook 'auto-fill-mode)

(hook-fn 'cb:org-minor-modes-hook
  (--each cb:org-minor-modes
    (ignore-errors (diminish it))))

(setq org-drawers '("COMMENTS" "NOTES" "PROPERTIES"
                    "CLOCK" "LOGBOOK" "RESULTS"))

(setq org-put-time-stamp-overlays t
      org-indirect-buffer-display 'current-window
      org-startup-indented t
      org-startup-with-inline-images t
      org-cycle-separator-lines 0)

(setq org-pretty-entities t)

(setq org-log-into-drawer t
      org-log-done 'time
      org-reverse-note-order nil)

(declare-modal-executor org-agenda-fullscreen
  :command (if cb-org:at-work?
               (org-agenda current-prefix-arg "w")
             (org-agenda current-prefix-arg "A")))

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

(defadvice org-add-log-note (before exit-minibuffer activate)
  (when (minibufferp (window-buffer (selected-window)))
    (other-window 1)))

(setq org-hierarchical-todo-statistics nil
      org-checkbox-hierarchical-statistics t)

(setq org-tag-persistent-alist
      '((:startgroup . nil)
        ("@computer" . ?c)
        ("@errand" . ?e)
        ("@home" . ?h)
        ("@leisure" . ?l)
        ("@phone" . ?p)
        ("@work" . ?w)
        (:endgroup . nil)))

(require 'org-clock)

(unless noninteractive
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate))

(setq org-clock-out-when-done t)

(setq org-clock-persist-query-resume nil)

(setq org-clock-into-drawer t)

(setq org-clock-history-length 20)

(setq org-clock-in-resume t
      org-clock-auto-clock-resolution 'when-no-clock-is-running)

(setq org-clock-report-include-clocking-task t)

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
  (cb-org:mark-next-parent-tasks-todo))

(setq org-clock-out-remove-zero-time-clocks t)

(hook-fn 'org-clock-out-hook
  :append t
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-to-list 'org-global-properties
             `("Effort_ALL" . ,(concat "1:00 2:00 3:00 4:00 "
                                       "5:00 6:00 7:00 8:00 9:00 "
                                       "0:05 0:10 0:30")))

(defvar org-work-capture-templates nil
  "Capture templates that should only be available when at work.")

(defvar org-home-capture-templates
  `(
    ("m" "Listening" entry
     (file+olp org-default-notes-file "Media" "Listening")
     "* MAYBE Listen to %i%?"
     :clock-keep t)

    ("v" "Viewing" entry
     (file+olp org-default-notes-file "Media" "Viewing")
     "* MAYBE Watch %i%?"
     :clock-keep t)

    ("r" "Reading" entry
     (file+olp org-default-notes-file "Media" "Reading")
     "* MAYBE Read %i%?"
     :clock-keep t)
    )
  "Capture templates that should only be available when not at work.")

(require 'org-capture)
(-each `(("t" "Todo" entry
          (file+olp org-default-notes-file "Tasks")
          "* TODO %?"
          :clock-keep t)

         ("d" "Diary" entry
          (file+datetree org-agenda-diary-file)
          "* %?\n%^t"
          :clock-keep t)

         ("h" "Habit" entry
          (file+olp org-default-notes-file "Habits/Recurring")
          ,(s-unlines
            "* TODO %?"
            "SCHEDULED: %t"
            ":PROPERTIES:"
            ":STYLE: habit"
            ":END:")
          :clock-keep t)

         ("l" "Link" entry
          (file+olp org-default-notes-file "Links")
          "* %c\n%i"
          :immediate-finish t
          :clock-keep t)

         ("s" "Someday" entry
          (file+olp org-default-notes-file "Someday")
          "* MAYBE %?"
          :clock-keep t)

         ("S" "Shopping" checkitem
          (file+olp org-default-notes-file "Tasks" "Shopping")
          "- [ ] %?"
          :clock-keep t)

         ("z" "Note" entry
          (file+olp org-default-notes-file "Notes")
          "* %i%?"
          :clock-keep t)
         )
  (~ add-to-list 'org-capture-templates))

(-each org-home-capture-templates (~ add-to-list 'org-capture-templates))

(defvar org-work-file (f-join org-directory "work.org")
  "Defines the path to file for work-related todos, etc.")

(defvar cb-org:work-persistence-file (f-join cb:tmp-dir ".org-at-work")
  "File to create that saves work state between sessions.")

(defvar cb-org:at-work? nil "Non-nil if currently 'at work'.")

(defun cb-org:goto-or-create-work-log ()
  (cl-destructuring-bind (_s _m _h d mo y &rest _)
      (decode-time (current-time))
    (org-datetree-find-date-create (list mo d y))))

(defun cb-org:start-work (file)
  (cb-org:set-notes-file file)
  (with-current-buffer (find-file-noselect file)
    (cb-org:goto-or-create-work-log)
    (org-clock-in))
  (setq cb-org:at-work? t)

  (setq org-capture-templates
        (-difference (-union org-capture-templates
                             org-work-capture-templates)
                     org-home-capture-templates))

  (f-touch cb-org:work-persistence-file)
  (message "Started work"))

(defun cb-org:leave-work (file)
  (with-current-buffer (find-file-noselect file)
    (cb-org:goto-or-create-work-log)
    (org-clock-out nil t))

  (cb-org:set-notes-file org-init-notes-file)
  (setq cb-org:at-work? nil)
  (setq org-capture-templates
        (-union (-difference org-capture-templates
                             org-work-capture-templates)
                org-home-capture-templates))

  (when (f-exists? cb-org:work-persistence-file)
    (f-delete cb-org:work-persistence-file))

  (message "Left work"))

(defun cb-org:toggle-at-work (file)
  "Toggle whether I am currently at work.
FILE is the file to use as the notes file while at work."
  (interactive (list org-work-file))
  (if (equal org-default-notes-file org-init-notes-file)
      (cb-org:start-work file)
    (cb-org:leave-work file))
  (when (derived-mode-p 'org-agenda-mode)
    (executor:org-agenda-fullscreen)))

(defun cb-org:maybe-set-to-work ()
  "Set status to 'at-work' if the work persistence file exists."
  (if (f-exists? cb-org:work-persistence-file)
      (cb-org:start-work org-work-file)
    (-each org-home-capture-templates
      (~ add-to-list 'org-capture-templates))))

(add-hook 'after-init-hook 'cb-org:maybe-set-to-work)

(bind-key "<f12>" 'cb-org:toggle-at-work)

(setq org-stuck-projects
      '("+project&TODO={.+}/-DONE-CANCELLED"
        ("NEXT" "TODO") nil "\\<IGNORE\\>"))

(setq org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))

(setq org-refile-target-verify-function
      (lambda ()
        (not (member (nth 2 (org-heading-components))
                     org-done-keywords))))

(require 'org-attach)
(setq org-link-abbrev-alist '(("att" . org-attach-expand-link))
      org-attach-directory (f-join org-directory "data"))

(defun org-attach-attach (file &optional visit-dir method)
  "Move/copy/link FILE into the attachment directory of the current task.
If VISIT-DIR is non-nil, visit the directory with dired. METHOD
may be `cp', `mv', `ln', or `lns' default taken from
`org-attach-method'."
  (interactive
   (list
    (ido-read-file-name "File to keep as an attachment: " nil nil t)
    current-prefix-arg))
  (setq method (or method org-attach-method))
  (let ((basename (file-name-nondirectory file)))
    (when (and org-attach-file-list-property (not org-attach-inherited))
      (org-entry-add-to-multivalued-property
       (point) org-attach-file-list-property basename))
    (let* ((attach-dir (org-attach-dir t))
           (fname (expand-file-name basename attach-dir)))
      (cond
       ((eq method 'mv) (rename-file file fname))
       ((eq method 'cp) (copy-file file fname))
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
        (message "File \"%s\" is now a task attachment." basename)))))

(require 'org-archive)

(defadvice org-archive-subtree
  (before add-inherited-tags-before-org-archive-subtree activate)
  "Add inherited tags before org-archive-subtree."
  (org-set-tags-to (org-get-tags-at)))

(defun cb-org:archive-done-tasks ()
  (interactive)
  (atomic-change-group
    (org-map-entries (lambda ()
                       ;; Ensure point does not move past the next item to
                       ;; archive.
                       (setq org-map-continue-from (point))
                       (org-archive-subtree))
                     "/DONE|PAID|VOID|CANCELLED" 'tree)))

(setq org-archive-default-command 'cb-org:archive-done-tasks)

(require 'org-crypt)

(org-crypt-use-before-save-magic)
(setq org-crypt-disable-auto-save 'encypt)

(add-to-list 'org-tags-exclude-from-inheritance "crypt")

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

(add-hook 'org-ctrl-c-ctrl-c-hook 'cb-org:decrypt-entry)

(require 'org-mime)

(define-key org-mode-map (kbd "C-c M-o") 'org-mime-org-buffer-htmlize)

(after 'message
  (define-key message-mode-map  (kbd "C-c M-o") 'org-mime-htmlize))

(hook-fn 'org-mime-html-hook
  (org-mime-change-element-style
   "blockquote" "border-left: 2px solid #B0B0B0; padding-left: 4px;")
  (org-mime-change-element-style
   "pre" "border-left: 2px solid #B0B0B0; padding-left: 4px;"))

(defun org-export-grab-title-from-buffer ()
  "")

(setq org-mime-default-header "#+OPTIONS: num:nil toc:nil latex:t\n")

(require 'cb-org-mail (f-join cb:lisp-dir "cb-org-mail"))

(define-command-picker mail-picker
  :title "*Mail Commands*"
  :options
  '(("m" "Compose Mail" org-compose-mail)
    ("s" "Compose Mail (subtree)" org-compose-mail-subtree :modes org-mode)))

(defun cb-compose-mail-dwim ()
  (interactive)
  (if (derived-mode-p 'org-mode)
      (call-interactively 'mail-picker)
    (call-interactively 'org-compose-mail)))

(bind-key* "C-x m" 'cb-compose-mail-dwim)

(defun org-narrow-to-subtree-content ()
  (widen)
  (unless (org-at-heading-p) (org-back-to-heading))
  (org-narrow-to-subtree)
  (forward-line)
  (narrow-to-region (line-beginning-position) (point-max)))

(defun org-subtree-content ()
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-write-subtree-content (dest)
  "Write the contents of the subtree at point to a file at DEST."
  (interactive (list (ido-read-file-name "Write subtree to: " nil nil nil ".org")))
  (f-write-text (org-subtree-content) 'utf-8 dest)
  (when (called-interactively-p nil)
    (message "Subtree written to %s" dest)))

(setq org-export-exclude-tags '("noexport" "crypt"))

(setq org-html-html5-fancy t)

(setq org-html-postamble nil)

(setq org-html-head-extra
      "
<style type=\"text/css\">
table tr.tr-odd td {
      background-color: #FCF6CF;
}
table tr.tr-even td {
      background-color: #FEFEF2;
}
</style>
")

(setq org-html-table-row-tags
      (cons '(cond (top-row-p "<tr class=\"tr-top\">")
                   (bottom-row-p "<tr class=\"tr-bottom\">")
                   (t (if (= (mod row-number 2) 1)
                          "<tr class=\"tr-odd\">"
                        "<tr class=\"tr-even\">")))
            "</tr>"))

(require 'ox-texinfo)
(add-to-list 'org-export-snippet-translation-alist
             '("info" . "texinfo"))

(require 'ox-koma-letter)
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
[PACKAGES]"))

(defun org-export-koma-letter-at-subtree (dest)
  "Define a command to export the koma letter subtree at point to PDF.
With a prefix arg, prompt for the output destination. Otherwise
generate use the name of the current file to generate the
exported file's name.
The PDF will be created at DEST."
  (interactive
   (list (if current-prefix-arg
             (ido-read-file-name "Destination: " nil nil nil ".pdf")
           (concat (f-no-ext (buffer-file-name)) ".pdf"))))

  (let ((tmpfile (make-temp-file "org-export-" nil ".org")))
    (org-write-subtree-content tmpfile)
    (with-current-buffer (find-file-noselect tmpfile)
      (unwind-protect
          (-if-let (exported (org-koma-letter-export-to-pdf))
              (f-move exported dest)
            (error "Export failed"))
        (kill-buffer)))
    (%-sh "open" (%-quote dest))
    (message "opening %s..." dest)))

(add-hook 'org-ctrl-c-ctrl-c-hook
          (lambda ()
            (when (ignore-errors
                    (s-matches? (rx "latex_class:" (* space) "koma")
                                (org-subtree-content)))
              (call-interactively 'org-export-koma-letter-at-subtree)
              'export-koma-letter))
          t)

(require 'org-habit)
(setq org-habit-preceding-days 14
      org-habit-following-days 4
      org-habit-graph-column 70)

(defun cb-org:ctrl-c-ctrl-k (&optional n)
  "Kill subtrees, unless we're in a special buffer where it should cancel."
  (interactive "p")
  (if (s-starts-with? "*Org" (buffer-name))
      (org-kill-note-or-show-branches)
    (org-cut-subtree n)))

(define-key org-mode-map (kbd "C-c C-k") 'cb-org:ctrl-c-ctrl-k)

(defun cb-org:ctrl-c-ret ()
  "Call `org-table-hline-and-move' or `org-insert-todo-heading' dep. on context."
  (interactive)
  (cond
   ((org-at-table-p) (call-interactively 'org-table-hline-and-move))
   (t (call-interactively 'org-insert-todo-heading))))

(define-key org-mode-map (kbd "C-c RET") 'cb-org:ctrl-c-ret)

(defun tidy-org-buffer ()
  "Perform cosmetic fixes to the current org-mode buffer."
  (save-restriction
    (org-table-map-tables 'org-table-align 'quiet)
    ;; Realign tags.
    (org-set-tags 4 t)
    ;; Remove empty properties drawers.
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp ":PROPERTIES:" nil t)
        (save-excursion
          (org-remove-empty-drawer-at "PROPERTIES" (match-beginning 0)))))))

(hook-fn 'org-mode-hook
  (add-hook 'before-save-hook 'tidy-org-buffer nil t))

(require 'org-agenda)

(add-hook 'org-agenda-mode-hook 'org-agenda-to-appt)

(add-to-list 'org-agenda-files org-directory)

(setq org-agenda-insert-diary-extract-time t)

(setq org-agenda-start-on-weekday nil
      org-agenda-span 'week
      org-agenda-ndays 7)

(defvar org-agenda-customise-window-hook nil
  "Relay hook for `org-agenda-mode-hook'. Suitable for setting up the window.")

(hook-fn 'org-agenda-mode-hook
  (run-hooks 'org-agenda-customise-window-hook))

(defvar cb-org:show-agenda-idle-delay (* 30 60)
  "The delay in seconds after which to pop up today's agenda.")

(defvar cb-org:show-agenda-idle-timer
  (unless noninteractive
    (run-with-idle-timer cb-org:show-agenda-idle-delay t
                         'executor:org-agenda-fullscreen))
  "Idle timer that will display today's org agenda.
See `cb-org:show-agenda-idle-delay'.")

(defun cb-org:exclude-tasks-on-hold (tag)
  (and (equal tag "hold") (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'cb-org:exclude-tasks-on-hold)

(setq org-agenda-text-search-extra-files '(agenda-archives))

(setq org-agenda-search-view-always-boolean t)

(setq org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-prewarning-if-scheduled t)

(setq org-agenda-hide-tags-regexp (rx (or "noexport" "someday")))

(setq org-agenda-show-inherited-tags nil)

(setq org-agenda-show-all-dates nil)

(setq org-agenda-custom-commands
      (->> '(("A" "Agenda and next actions"
              ((tags-todo "-someday-media/NEXT"
                          ((org-agenda-overriding-header "Next Actions")))
               (agenda "")
               (todo "WAITING"
                     ((org-agenda-overriding-header "Waiting")))
               (tags-todo "media/NEXT"
                          ((org-agenda-overriding-header "Media"))))
              ((org-agenda-tag-filter-preset '("-@work"))))

             ("w" "Agenda and work actions"
              ((tags-todo "-someday-PeterBarrett-PeterRoberts/NEXT"
                          ((org-agenda-overriding-header "Next Actions")))
               (agenda "" ((org-agenda-ndays 14)))
               (todo "AWAITING|ORGANISE_IN"
                     ((org-agenda-overriding-header "Incoming")))
               (todo "TODO_OUT|READY|ORGANISE_OUT"
                     ((org-agenda-overriding-header "Outgoing")))
               (todo "CALLBACK|MESSAGE"
                     ((org-agenda-overriding-header "Phone calls")))
               (tags-todo "-someday/WAITING"
                          ((org-agenda-overriding-header "Waiting")))
               (tags-todo "-someday+PeterBarrett"
                          ((org-agenda-overriding-header "Peter Barrett")))
               (tags-todo "-someday+PeterRoberts"
                          ((org-agenda-overriding-header "Peter Roberts")))
               )

              ((org-agenda-tag-filter-preset '("+@work"))
               (org-agenda-files (list org-work-file))
               (org-agenda-hide-tags-regexp
                (regexp-opt (list org-agenda-hide-tags-regexp "@work")))))

             ("n" "Next actions"
              ((tags-todo "-someday/NEXT"))
              ((org-agenda-overriding-header "Next Actions")))

             ("g" . "GTD contexts")
             ("gg" "Anywhere"
              ((tags-todo "@computer")
               (tags-todo "@errand")
               (tags-todo "@home")
               (tags-todo "@leisure")
               (tags-todo "@phone")
               (tags-todo "@work")))
             ("gc" "Computer" tags-todo "@computer")
             ("ge" "Errands"  tags-todo "@errand")
             ("gp" "Phone"    tags-todo "@phone")
             ("gw" "Work"     tags-todo "@work")
             ("gh" "Home"     tags-todo "@home")
             ("gl" "Leisure"  tags-todo "@leisure")

             ("r" "Weekly Review"
              ((agenda "" ((org-agenda-ndays 14)))
               (stuck "")
               (todo "WAITING"
                     ((org-agenda-overriding-header "Waiting")))
               (tags-todo "someday-skill/MAYBE|NEXT"
                          ((org-agenda-overriding-header "Someday")))
               (tags-todo "someday&skill"
                          ((org-agenda-overriding-header "Skills")))
               (tags-todo "media"
                          ((org-agenda-overriding-header "Media"))))

              ((org-agenda-tag-filter-preset '("-drill" "-gtd"))
               (org-habit-show-habits nil)))

             ("g" . "GTD contexts")
             ("gg" "Anywhere"
              ((tags-todo "@computer")
               (tags-todo "@errand")
               (tags-todo "@home")
               (tags-todo "@leisure")
               (tags-todo "@phone")
               (tags-todo "@work")))
             ("gc" "Computer" tags-todo "@computer")
             ("ge" "Errands"  tags-todo "@errand")
             ("gp" "Phone"    tags-todo "@phone")
             ("gw" "Work"     tags-todo "@work")
             ("gh" "Home"     tags-todo "@home")
             ("gl" "Leisure"  tags-todo "@leisure"))
        (--map-when (listp (cdr it))
                    (append it
                            '(((org-agenda-customise-window-hook
                                'delete-other-windows)))))))

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up priority-down category-keep)
        (todo priority-down category-keep scheduled-up)
        (tags priority-down category-keep)
        (search category-keep)))

(defun cb-org:agenda-next-section ()
  "Move to the next section in the agenda."
  (interactive)
  (save-match-data
    (cond ((search-forward-regexp (rx bol (+ "="))
                                  nil t)
           (goto-char (line-beginning-position))
           (org-agenda-next-item 1))
          (t
           (goto-char (point-max))
           (goto-char (line-beginning-position))))))

(defun cb-org:agenda-prev-section ()
  "Move to the previous section in the agenda."
  (interactive)
  (save-match-data
    (cl-flet ((goto-section-start ()
                                  (when (search-backward-regexp (rx bol (+ "=")) nil t)
                                    (org-agenda-next-item 1)
                                    (goto-char (line-beginning-position))
                                    (point))))
      (let ((current-section-start (save-excursion (goto-section-start))))
        (cond
         ((and (equal (point) current-section-start)
               (search-backward-regexp (rx bol (+ "=")) nil t 2)))
         ((search-backward-regexp (rx bol (+ "=")) nil t))
         (t
          (goto-char (point-min)))

         (forward-line 1)
         (goto-char (line-beginning-position)))))))

(define-key org-agenda-mode-map (kbd "M-N") 'cb-org:agenda-next-section)
(define-key org-agenda-mode-map (kbd "M-P") 'cb-org:agenda-prev-section)

(after 'smartparens
  (hook-fn 'org-agenda-mode-hook
    (smartparens-mode -1)))

(defun cb-org:agenda-auto-exclude (tag)
  "Hide drills."
  (when (equal "drill" tag)
    (concat "-" tag)))

(defun cb-org:apply-auto-exclude ()
  (org-agenda-filter-by-tag nil ?\r t))

(add-hook 'org-agenda-mode-hook 'cb-org:apply-auto-exclude)
(setq org-agenda-auto-exclude-function 'cb-org:agenda-auto-exclude)

(defvar date nil
  "Dynamic var bound to current date by calendaring functions.")

(autoload 'calendar-extract-year "calendar")
(autoload 'calendar-day-number "calendar")
(autoload 'calendar-day-of-week "calendar")

(defun calendar-nearest-to (target-dayname target-day target-month)
  "Non-nil if the current date is a certain weekday close to an anniversary.

TARGET-DAYNAME is the day of the week that we want to match,
 while TARGET-DAY and TARGET-MONTH are the anniversary."
  (let* ((dayname (calendar-day-of-week date))
         (target-date (list target-month target-day (calendar-extract-year date)))
         (days-diff (abs (- (calendar-day-number date)
                            (calendar-day-number target-date)))))
    (and (= dayname target-dayname)
         (< days-diff 4))))

(defun calendar-mondayised (target-day target-month)
  "Given anniversary with DAY and MONTH, return non-nil if:

- the given date is a weekday, or

- it is the Monday after the given date if it falls on a weekend."
  (if (memq (calendar-day-of-week date) '(6 0)) ; Sat or Sun
      (calendar-nearest-to 1 target-day target-month)

    (let ((m (calendar-extract-month date))
          (d (calendar-extract-day date)))
      (and (equal d target-day)
           (equal m target-month)))) )

(defun diary-limited-cyclic (recurrences interval m d y)
  "For use in emacs diary. Cyclic item with limited number of recurrences.
Occurs every INTERVAL days, starting on YYYY-MM-DD, for a total of
RECURRENCES occasions."
  (let ((startdate (calendar-absolute-from-gregorian (list m d y)))
        (today (calendar-absolute-from-gregorian date)))
    (and (not (cl-minusp (- today startdate)))
         (zerop (% (- today startdate) interval))
         (< (floor (- today startdate) interval) recurrences))))

(cl-defun cb-org:format-class-sexpr ((s1 m1 h1 d1 m1 y1 . _)
                                     (s2 m2 h2 d2 m2 y2 . _)
                                     desc)
  "Parse dates into an org-class s-expression."
  (let* ((time (unless (or (zerop m1) (zerop h1))
                 (format " %.2i:%.2i %s" h1 m1 desc)))
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
     desc)))

(defun org-insert-class ()
  "Read and insert a class diary sexp at point."
  (interactive "*")
  (insert (org-read-class)))

(defun calendar-easter-date (year)
  "Calculate the date for Easter Sunday in YEAR. Returns the date in the
Gregorian calendar, ie (MM DD YY) format."
  (let* ((century (1+ (/ year 100)))
         (shifted-epact (% (+ 14 (* 11 (% year 19))
                              (- (/ (* 3 century) 4))
                              (/ (+ 5 (* 8 century)) 25)
                              (* 30 century))
                           30))
         (adjusted-epact (if (or (= shifted-epact 0)
                                 (and (= shifted-epact 1)
                                      (< 10 (% year 19))))
                             (1+ shifted-epact)
                           shifted-epact))
         (paschal-moon (- (calendar-absolute-from-gregorian
                           (list 4 19 year))
                          adjusted-epact)))
    (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))

(defun calendar-days-from-easter ()
  "When used in a diary sexp, this function will calculate how many days
are between the current date (DATE) and Easter Sunday."
  (- (calendar-absolute-from-gregorian date)
     (calendar-easter-date (calendar-extract-year date))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ledger . t)
   (C . t)
   (ditaa . t)
   (sh . t)
   (calc . t)
   (scala . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (ruby . t)
   (clojure . t)
   (haskell . t)))

(setq org-src-fontify-natively t)

(setq org-confirm-babel-evaluate nil)

(setq org-edit-src-content-indentation 0)

(defvar org-edit-src-before-exit-hook nil
  "Hook run before exiting a code block.")

(defadvice org-edit-src-exit (before run-hook activate)
  (run-hooks 'org-edit-src-before-exit-hook))

(hook-fn 'org-src-mode-hook
  (setq-local require-final-newline nil))

(add-hook 'org-edit-src-before-exit-hook 'delete-trailing-whitespace)

(setq org-enforce-todo-dependencies t)

(setq org-todo-keywords
      '((type "MAYBE(m)" "TODO(t)" "NEXT(n)" "WAITING(w@/!)"
              "|" "DONE(d!)" "CANCELLED(c@)" "DELEGATED(D@)")))

(defface org-todo-next
  `((((background dark))
     (:foreground ,solarized-hl-orange :bold t))
    (((background light))
     (:foreground ,solarized-hl-orange :bold t))
    (t
     (:inherit org-todo)))
  "Face for todos with the NEXT label."
  :group 'org-faces)

(defface org-todo-out
  `((t (:foreground ,solarized-hl-orange :bold t :inherit org-todo)))
  "Face for TODO_OUT keyword."
  :group 'org-faces)

(defface org-todo-ready
  `((t (:foreground ,solarized-hl-blue :bold t :inherit org-todo)))
  "Face for TODO_OUT keyword."
  :group 'org-faces)

(setq org-todo-keyword-faces
      '(("NEXT" . org-todo-next)
        ("ORGANISE_IN" . org-todo-next)
        ("ORGANISE_OUT" . org-todo-next)
        ("TODO_OUT" . org-todo-out)
        ("READY" . org-todo-ready)))

(hook-fn 'org-after-todo-statistics-hook
  :arglist (n-done n-not-done)
  (let (org-log-done org-log-states) ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun cb-org:set-next-todo-state ()
  "When marking a todo to DONE, set the next TODO as NEXT.
Do not change habits, scheduled items or repeating todos."
  (when (equal org-state "DONE")
    (save-excursion
      (when (and (ignore-errors (outline-forward-same-level 1) t)
                 (equal (org-get-todo-state) "TODO"))
        (unless (or (org-is-habit-p)
                    (org-entry-get (point) "STYLE")
                    (org-entry-get (point) "LAST_REPEAT")
                    (org-get-scheduled-time (point)))
          (org-todo "NEXT"))))))

(add-hook 'org-after-todo-state-change-hook 'cb-org:set-next-todo-state)

(cb:install-package 'orglink t)

(setq orglink-activate-links '(angle plain))

(setq orglink-mode-lighter nil)

(hook-fns '(prog-mode-hook text-mode-hook comint-mode)
  (ignore-errors
    (unless (derived-mode-p 'org-mode 'nxml-mode 'sgml-mode 'snippet-mode)
      (orglink-mode +1))))

(autoload 'org-pomodoro "org-pomodoro")
(bind-key* "<f5>" 'org-pomodoro)

(setq org-pomodoro-long-break-length 25)

(setq org-pomodoro-format "• %s"
      org-pomodoro-short-break-format "Break %s"
      org-pomodoro-long-break-format "Break %s"
      org-pomodoro-show-seconds nil)

(setq org-pomodoro-show-in-mode-line nil)

(require 'appt)
(setq appt-message-warning-time 60
      appt-display-interval 15)

(save-window-excursion
  (appt-activate +1))

(defun cb-org:save-diary ()
  (save-restriction
    (save-window-excursion
      (org-agenda-to-appt t)
      (appt-check 'force))))

(hook-fn 'org-mode-hook
  (when (equal (buffer-file-name) org-agenda-diary-file)
    (add-hook 'after-save-hook 'cb-org:save-diary nil t)))

(defvar cb-org:appt-update-timer
  (run-with-idle-timer 240 t 'org-agenda-to-appt t))

(require 'org-protocol)

(cb:install-package 'ox-reveal t)

(setq org-reveal-root (concat "file://" (f-join cb:lib-dir "reveal.js")))

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
   :isearch t))

(setq org-drill-save-buffers-after-drill-sessions-p nil)

(defadvice org-drill (after save-buffers activate)
  (org-save-all-org-buffers))

(--each '(org-drill
          org-drill-strip-all-data
          org-drill-cram
          org-drill-tree
          org-drill-resume
          org-drill-merge-buffers
          org-drill-entry
          org-drill-directory
          org-drill-again)
  (autoload it "org-drill" nil t))

(cb:install-package 'org-drill-table t)
(add-hook 'org-ctrl-c-ctrl-c-hook 'org-drill-table-update)

(defun cb-org:drill-buffer? ()
  "Non-nil if the current buffer contains any drill items."
  (and
   (derived-mode-p 'org-mode)
   (s-matches? ":drill:" (buffer-string))))

(add-to-list 'org-action-picker-options
             '("r" "Org Drill" (lambda () (org-drill 'agenda))))

(after 'org
  (require 'iimage))

(after 'iimage
  (add-to-list 'iimage-mode-image-regex-alist
               (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
                             "\\)\\]")  1)))

(defun org-toggle-iimage-in-org ()
  "Display images in the current orgmode buffer."
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
    (set-face-underline-p 'org-link t))
  (iimage-mode))

(add-to-list 'org-structure-template-alist
             '("n" "#+begin_nospell\n?\n#+end_nospell" "?"))

(defun cb-org:in-no-spellcheck-zone? ()
  (thing-at-point-looking-at (rx "#+begin_nospell" (*? anything ) "#+end_nospell")))

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

(setq org-structure-template-alist
      (-map (~ -map 's-downcase) org-structure-template-alist))

(after 'auto-complete
  (hook-fn 'org-mode-hook
    (setq-local ac-sources nil)
    (auto-complete-mode -1)))

(after 'evil

(add-hook 'org-capture-mode-hook 'cb:maybe-evil-insert-state)

(hook-fn 'org-mode-hook
  (when (equal (buffer-name) "*Org Note*")
    (cb:maybe-evil-insert-state)))

(after 'org-agenda
  (bind-keys
    :map org-agenda-mode-map
    "C" 'org-agenda-capture
    "g" 'org-agenda-goto-date
    "j" 'org-agenda-next-item
    "k" 'org-agenda-previous-item
    "L" 'org-agenda-log-mode
    "l" 'evil-forward-char
    "h" 'evil-backward-char
    "C-f" 'evil-scroll-page-down
    "C-b" 'evil-scroll-page-up))

(defadvice org-insert-heading (after insert-state activate)
  (when (called-interactively-p nil)
    (cb:maybe-evil-insert-state)))

(defadvice org-insert-heading-respect-content (after insert-state activate)
  (when (called-interactively-p nil)
    (cb:maybe-evil-insert-state)))

(defadvice org-insert-todo-heading (after insert-state activate)
  (when (called-interactively-p nil)
    (cb:maybe-evil-insert-state)))

(defadvice org-insert-todo-heading-respect-content (after insert-state activate)
  (when (called-interactively-p nil)
    (cb:maybe-evil-insert-state)))

(defadvice org-toggle-heading (after goto-line-end activate)
  "Prevent point from being moved to the line beginning."
  (when (s-matches? (rx bol (+ "*") (* space) eol) (current-line))
    (goto-char (line-end-position))))

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

(defadvice org-return (around newlines-only-in-insert-state activate)
  "Only insert newlines if we're in insert state."
  (noflet ((newline (&rest args)
                    (when (and (fboundp 'evil-insert-state-p)
                               (evil-insert-state-p))
                      (funcall this-fn args))))
      ad-do-it))

)

(defconst org-unicorn-png
  (f-join user-emacs-directory "assets" "org_unicorn.png"))

(when (equal system-type 'darwin)

(defun cb-appt:growl (title mins)
  (growl (cond ((zerop mins) "Appointment (now)")
               ((= 1 mins)   "Appointment (1 min)")
               (t (format "Appointment (%s mins)" mins)))
         (cl-destructuring-bind (whole time desc)
             (s-match (rx bol
                          (group (+ digit) ":" (+ digit))
                          (* space)
                          (group (* nonl)))
                      title)
           desc)))

(defadvice appt-display-message (around growl-with-sound activate)
  "Play a sound and display a growl notification for appt alerts."
  ;; Show notification.
  (let ((title (-listify (ad-get-arg 0)))
        (mins (-listify (ad-get-arg 1))))
    (-each (-zip-with 'list title mins)
           (-applify 'cb-appt:growl)))
  ;; Play sound.
  (osx-play-system-sound "blow"))

(hook-fn 'org-timer-start-hook (growl "Timer Started" "" org-unicorn-png))
(hook-fn 'org-timer-done-hook (growl "Timer Finished" "" org-unicorn-png))
(hook-fn 'org-timer-done-hook (osx-play-system-sound "glass"))

(defadvice org-protocol-do-capture (after show-growl-notification activate)
  "Show Growl notification when capturing links."
  (let* ((parts (org-protocol-split-data (ad-get-arg 0) t org-protocol-data-separator))
         ;; Pop the template selector if present.
         (template (or (and (>= 2 (length (car parts))) (pop parts))
                       org-protocol-default-template-key))
         (url (org-protocol-sanitize-uri (car parts)))
         (type (if (string-match "^\\([a-z]+\\):" url)
                   (match-string 1 url)))
         (title (or (cadr parts) "")))
    (growl "Link Stored" (or title url) org-unicorn-png)))

(let ((snd (osx-find-system-sound "purr")))
  (setq org-pomodoro-sound snd
        org-pomodoro-short-break-sound snd
        org-pomodoro-long-break-sound snd))

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

(add-hook 'org-pomodoro-finished-hook 'cb-org:pomodoro-growl)
(add-hook 'org-pomodoro-started-hook 'cb-org:pomodoro-growl)
(add-hook 'org-pomodoro-killed-hook 'cb-org:pomodoro-growl)

(defun cb-org:pomodoro-growl-end-break ()
  (growl "Pomodoro"
         "Break finished"
         (f-join cb:assets-dir "org-pomodoro.png")))

(add-hook 'org-pomodoro-short-break-finished-hook 'cb-org:pomodoro-growl-end-break)
(add-hook 'org-pomodoro-short-break-finished-hook 'cb-org:pomodoro-growl-end-break)

)

(when (or (daemonp) (display-graphic-p))
  (hook-fn 'after-init-hook
    (unless noninteractive
      (executor:org-agenda-fullscreen))))

(defvar cb-org:notes-save-idle-delay 60)

(defun cb-org:save-notes ()
  "Save the notes file."
  (-when-let (buf (--first-buffer (equal (buffer-file-name it)
                                         org-default-notes-file)))
    (with-current-buffer buf
      (when (buffer-modified-p)
        (save-buffer)))))

(defvar cb-org:notes-save-timer
  (unless noninteractive
    (run-with-idle-timer cb-org:notes-save-idle-delay t 'cb-org:save-notes))
  "Timer that automatically saves the notes buffer on idle.")

(add-hook 'org-mode-hook 'abbrev-mode)

(require 'capture-mail)

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
      (run-with-timer 5 60 (lambda ()
                             (capture-mail cm-capture-messages-dir))))))

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

(bind-keys
  :overriding? t
  "C-c a" 'org-agenda
  "C-c l" 'org-store-link
  "<f6>" 'cb-org:quick-capture
  "<f7>" 'cb-org:capture
  "<f8>" 'org-action-picker
  "<f9>" 'executor:org-agenda-fullscreen)

(define-keys org-mode-map
  "C-c C-." 'org-time-stamp-inactive
  "C-c o" 'org-attach-open
  "M-p" 'org-metaup
  "M-n" 'org-metadown
  "C-c c" 'org-columns
  "C-c C-j" (command (org-refile 'goto))
  ;; disable annoying comment toggle key
  "C-c ;" nil)

(when (executable-find "gnuplot")
  (cb:install-package 'gnuplot)
  (define-key org-mode-map (kbd "M-C-g") 'org-plot/gnuplot))

(defun org-latex-wrap ()
  (interactive)
  (let ((r (current-region)))
    (delete-region (region-beginning) (region-end))
    (insert (format "@@latex:%s@@" r))))

(provide 'config-orgmode)

;;; config-orgmode.el ends here
