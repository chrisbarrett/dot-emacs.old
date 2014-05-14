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
(require 'config-theme)
(require 'config-modegroups)
(require 'config-darwin)
(require 'org)
(autoload 'org-agenda-filter-apply "org-agenda")
(autoload 'org-is-habit-p "org-habit")

(defvar org-init-notes-file org-default-notes-file
  "Captures the original value of the `org-default-notes-file'.")

(custom-set-variables
 '(org-M-RET-may-split-line nil)
 '(org-attach-directory (f-join org-directory "data"))
 '(org-blank-before-new-entry nil)
 '(org-catch-invisible-edits 'smart)
 '(org-clock-persist-file (f-join org-directory ".org-clock-save"))
 '(org-completion-use-ido t)
 '(org-cycle-separator-lines 0)
 '(org-drawers '("COMMENTS" "NOTES" "PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS"))
 '(org-enforce-todo-dependencies t)
 '(org-footnote-auto-adjust t)
 '(org-id-locations-file (f-join cb:tmp-dir "org-id-locations"))
 '(org-indirect-buffer-display 'current-window)
 '(org-insert-heading-respect-content nil)
 '(org-link-abbrev-alist '(("att" . org-attach-expand-link)))
 '(org-link-mailto-program '(compose-mail "%a" "%s"))
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-outline-path-complete-in-steps nil)
 '(org-pretty-entities t)
 '(org-put-time-stamp-overlays t)
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-refile-target-verify-function (lambda () (not (member (nth 2 (org-heading-components)) org-done-keywords))))
 '(org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
 '(org-refile-use-outline-path t)
 '(org-return-follows-link t)
 '(org-reverse-note-order nil)
 '(org-startup-indented t)
 '(org-startup-with-inline-images t)
 '(org-stuck-projects '("+project&TODO={.+}/-DONE-CANCELLED" ("NEXT" "TODO") nil "\\<IGNORE\\>"))
 '(org-support-shift-select t)
 '(org-todo-keywords '((type "MAYBE(m)" "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)" "DELEGATED(D@)")))
 '(org-hierarchical-todo-statistics nil)
 '(org-checkbox-hierarchical-statistics t)
 '(org-tag-persistent-alist
   '((:startgroup)
     ("@computer" . 99)
     ("@errand" . 101)
     ("@home" . 104)
     ("@leisure" . 108)
     ("@phone" . 112)
     ("@work" . 119)
     (:endgroup))))

(add-hook 'org-mode-hook 'auto-revert-mode)

(add-hook 'org-mode-hook 'auto-fill-mode)

(hook-fn 'cb:org-minor-modes-hook
  (--each cb:org-minor-modes
    (ignore-errors (diminish it))))

(declare-modal-executor org-agenda-fullscreen
  :command (if (true? cb-org:at-work?)
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
    ("V" "View Tags (all)" executor:org-tags-view-all-fullscreen)))

(defadvice org-add-log-note (before exit-minibuffer activate)
  "If the minibuffer is active, exit before prompting for a note."
  (when (minibufferp (window-buffer (selected-window)))
    (other-window 1)))

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

(require 'org-attach)

(defun org-attach-attach (file &optional visit-dir method)
  "Move/copy/link FILE into the attachment directory of the current task.

If VISIT-DIR is non-nil, visit the directory with dired.

METHOD may be `cp', `mv', `ln', or `lns' default taken from
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

(require 'cb-org-mail)

(define-command-picker mail-picker
  :title "*Mail Commands*"
  :options
  '(("m" "Compose Mail" org-compose-mail)
    ("s" "Compose Mail (subtree)" org-compose-mail-subtree :modes org-mode)))

(defun cb-compose-mail-dwim ()
  "Either compose a new message immediately or show composition options."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (call-interactively 'mail-picker)
    (call-interactively 'org-compose-mail)))

(bind-key* "C-x m" 'cb-compose-mail-dwim)

(defun org-narrow-to-subtree-content ()
  "Narrow to the content of the subtree.  Excludes the heading line."
  (widen)
  (unless (org-at-heading-p) (org-back-to-heading))
  (org-narrow-to-subtree)
  (forward-line)
  (narrow-to-region (line-beginning-position) (point-max)))

(defun org-subtree-content ()
  "Return the content of the subtree at point as a string."
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
  "Perform cosmetic fixes to the current org buffer."
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

(require 'iimage)
(add-to-list 'iimage-mode-image-regex-alist
             (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
                           "\\)\\]")  1))

(defun org-toggle-iimage-in-org ()
  "Display images in the current orgmode buffer."
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline 'org-link nil)
    (set-face-underline 'org-link t))
  (iimage-mode))

(add-to-list 'org-structure-template-alist
             '("n" "#+begin_nospell\n?\n#+end_nospell" "?"))

(setq org-structure-template-alist
      (-map (~ -map 's-downcase) org-structure-template-alist))

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
  (hook-fn 'org-timer-start-hook (growl "Timer Started" "" org-unicorn-png))
  (hook-fn 'org-timer-done-hook (growl "Timer Finished" "" org-unicorn-png))
  (hook-fn 'org-timer-done-hook (osx-play-system-sound "glass")))

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

(defun org-latex-wrap (beg end)
  "Wrap the current region from BEG to END in a latex directive."
  (interactive "r")
  (let ((r (buffer-substring beg end)))
    (delete-region (region-beginning) (region-end))
    (insert (format "@@latex:%s@@" r))))

(provide 'config-orgmode)

;;; config-orgmode.el ends here
