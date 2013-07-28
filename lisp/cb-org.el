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
(require 'f)
(autoload 'projectile-project-root "projectile")
(autoload 'projectile-project-p "projectile")

(defvar org-directory (f-join user-home-directory "org"))
(defvar org-default-notes-file (f-join org-directory "notes.org"))
(defvar org-id-locations-file (f-join cb:tmp-dir "org-id-locations"))
(defvar org-clock-persist-file (f-join user-dropbox-directory ".org-clock-save.el"))
(defvar org-export-publishing-directory (f-join user-home-directory "Desktop"))
(defvar org-agenda-diary-file (f-join org-directory "diary.org"))
(defvar org-export-exclude-tags '("noexport" "crypt"))
(defvar org-last-project-task-file nil)
(defvar calendar-date-style 'european)

(declare-modal-executor org-agenda-fullscreen
  :bind "M-O"
  :command cb-org:show-agenda-list)

(defmacro with-org-default-notes-buffer (&rest body)
  "Perform BODY with the `org-defaut-notes-buffer' set to current."
  (declare (indent 0))
  (let ((buf (cl-gensym)))
    `(-when-let (,buf (--first-buffer (equal (buffer-file-name it)
                                             org-default-notes-file)))
       (with-current-buffer ,buf
         ,@body))))

(defun cb-org:project-task-file ()
  "Get the path to a task file at the root directory of the current project."
  (-when-let (proj (or (ignore-errors (projectile-project-root))
                       (with-current-buffer (--first-buffer (projectile-project-p))
                         (projectile-project-root))))
    (f-join proj "Tasks.org")))

(defun cb-org:show-todo-list ()
  "Show the todo list.
Make the 'q' key restore the previous window configuration."
  (interactive)
  (with-window-restore
    (org-todo-list)
    (with-current-buffer
        (window-buffer (--first-window
                        (with-current-buffer (window-buffer it)
                          (derived-mode-p 'org-agenda-mode))))
      (buffer-local-set-key (kbd "q") (command (restore))))))

(defun cb-org:show-agenda-list ()
  "Show the todo list.
Make the 'q' key restore the previous window configuration."
  (interactive)
  (with-window-restore
    (org-agenda-list nil nil 1)
    (with-current-buffer
        (window-buffer (--first-window
                        (with-current-buffer (window-buffer it)
                          (derived-mode-p 'org-agenda-mode))))
      (buffer-local-set-key (kbd "q") (command (restore))))))

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

(defun cb-org:format-project-task-file ()
  "Ensure the current project tasks file has its metadata fields set.
Non-nil if modifications where made."
  (save-excursion
    (goto-char (point-min))
    ;; Skip file variables.
    (when (emr-line-matches? (rx "-*-" (* nonl) "-*-"))
      (forward-line))
    (beginning-of-line)
    (let ((changed? (list
                     (cb-org:ensure-field "#+TITLE: Tasks")
                     (cb-org:ensure-field (concat "#+AUTHOR: " user-full-name))
                     (cb-org:ensure-field "#+STARTUP: lognotestate" t)
                     (cb-org:ensure-field "#+STARTUP: lognotedone" t)
                     (cb-org:ensure-field "#+DESCRIPTION: Project-level notes and todos"))))
      (-any? 'identity changed?))))

(after 'smartparens
  (sp-with-modes '(org-mode)
    (sp-local-pair "#+BEGIN_SRC" "#+END_SRC")
    (sp-local-pair "#+begin_src" "#+end_src")))

(defun org-forward-move-past-headers ()
  "Move point past the header lines of an org document."
  (while (or (emr-line-matches? (rx bol (or "#+" "-*-")))
             (emr-blank-line?))
    (unless (eobp)
      (forward-line))))

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
      (evil-global-set-key 'normal (kbd "C-o") 'cb-org-map))

    (bind-keys
      :overriding? t
      :map cb-org-map
      "C-o" 'cb-org-map
      "C-o a" 'cb-org:show-agenda-list
      "C-o d" (command (find-file org-agenda-diary-file))
      "C-o c" 'org-capture
      "C-o K" (command (org-capture nil "T"))
      "C-o k" (command (org-capture nil "t"))
      "C-o n" (command (find-file org-default-notes-file))
      "C-o p" (command (when (find-file (or (cb-org:project-task-file)
                                            org-last-project-task-file
                                            (user-error "Not in a project")))
                         (cb-org:format-project-task-file)))
      "C-o t" 'cb-org:show-todo-list))

  :config
  (progn
    (setq org-modules '(org-bbdb org-crypt org-w3m org-habit)
          org-startup-indented t
          org-log-into-drawer t
          org-log-done 'time
          org-reverse-note-order nil
          org-return-follows-link t)

    (evil-define-keys 'normal org-mode-map
      "<return>" 'org-return
      "M-P" 'outline-previous-visible-heading
      "M-N" 'outline-next-visible-heading
      "SPC" 'org-cycle
      "z m" (command (org-global-cycle 1))
      "z r" (command (org-global-cycle 0)))

;;;; Hooks

    ;; Enter insert state for popup notes.
    (hook-fn 'org-mode-hook
      (when (and (equal (buffer-name) "*Org Note*"))
        (cb:append-buffer)))

    (add-hook 'org-mode-hook 'org-forward-move-past-headers)

    ;; Diminish org minor modes.
    (hook-fn 'cb:org-minor-modes-hook
      (--each cb:org-minor-modes
        (ignore-errors (diminish it))))

;;;; Auto-save notes file

    (defvar cb-org:notes-save-idle-delay 40)

    (defvar cb-org:notes-save-timer
      (run-with-idle-timer cb-org:notes-save-idle-delay t 'cb-org:save-notes)
      "Timer that automatically saves the notes buffer on idle.")

    (defun cb-org:save-notes ()
      "Save the notes file."
      (with-org-default-notes-buffer
        (when (buffer-modified-p)
          (save-buffer))))

;;;; Editing commands

    (defun org-insert-inactive-timestamp (&optional arg)
      (interactive "p")
      (org-time-stamp arg t))

    (define-key org-mode-map (kbd "C-c C-.") 'org-insert-inactive-timestamp)

;;;; Tasks

    (defun cb-org:show-task-file ()
      (switch-to-buffer
       (or (--first-buffer (equal (buffer-file-name) (cb-org:project-task-file)))
           (find-file-noselect (cb-org:project-task-file)))))

    (defun cb-org:task-file-contents ()
      (save-excursion
        (-if-let (buf (--first-buffer (equal (buffer-file-name) (cb-org:project-task-file))))
          (with-current-buffer buf
            (font-lock-fontify-buffer)
            (buffer-string))
          (with-current-buffer (find-file-noselect (cb-org:project-task-file) t)
            (prog1 (buffer-string)
              (kill-buffer))))))

    (defun cb-org:show-tasks (&optional arg)
      "Display the Tasks tree in the `cb-org:project-task-file'.
With prefix argument ARG, show the file and move to the tasks tree."
      (interactive "P")
      (if arg
          (cb-org:show-task-file)
        (let ((str (cb-org:task-file-contents)))
          (if (emr-blank? str)
              (user-error "No current tasks")
            (message (s-trim str))))))

    (bind-key* "M-?" 'cb-org:show-tasks)

;;;; Org babel

    (setq org-src-fontify-natively t
          org-confirm-babel-evaluate nil)

    (org-babel-do-load-languages
     'org-babel-load-languages '((python . t)
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
             "NEXT(n@/!)" "TODO(t)" "WAITING(w@/!)" "SOMEDAY(s)"
             "|" "DONE(d!)" "DEFERRED" "CANCELLED(c@)"))

          org-todo-keyword-faces
          '(("NEXT" . org-todo-next)))

    (--each '("NOTES" "COMMENTS")
      (add-to-list 'org-drawers it))

    (add-hook 'org-mode-hook 'auto-revert-mode)

    (define-keys org-mode-map
      "M-p" 'org-metaup
      "M-n" 'org-metadown)

    (defadvice org-add-log-note (before exit-minibuffer activate)
      "Prevent attempts to expand the minibuffer."
      (when (minibufferp (window-buffer (selected-window)))
        (other-window 1)))))

(use-package org-capture
  :commands (org-capture)
  :config
  (progn

 ;;;; Diary

    (defun cb-org:read-diary-entry ()
      "Read info from the user to construct a new diary entry."
      (save-window-excursion
        (let ((desc (s-trim (read-string "Description: " nil t)))
              (date (org-read-date)))
          (concat "* " desc "\n"
                  "  <" date ">"))))

 ;;;; TODOS

    (defun* cb-org:read-todo (&optional (prompt "TODO: "))
      "Read a todo item for org-capture."
      (save-window-excursion
        (let ((desc (s-trim (read-string prompt nil t)))
              (start (org-read-date)))
          (concat "* TODO " desc "\n"
                  "  SCHEDULED: <" start "> \n"))))

    (defun cb-org:read-project-task ()
      "Read info from the user to construct a task for the current project."
      (save-window-excursion
        (-if-let (f (cb-org:project-task-file))
          (prog1 (cb-org:read-todo (format "TODO [%s]: " (f-short f)))
            (setq org-last-project-task-file f))
          (user-error "Not in a project"))))

;;;; Org Habits

    (defun cb-org:time-freq->range-fmt (str)
      "Turn an English representation of a habit string into a habit time format."
      ;; Normalise ranges in string.
      (let ((str (->> str
                   (s-trim)
                   (s-downcase)
                   (s-replace "every" "")
                   (s-replace "or" "-")
                   (s-replace "to" "-")
                   (s-replace "/"  "-"))))
        (or (and (s-matches? (rx bol (* space) (or "daily" "day") (* space) eol)
                             str)
                 "+1d")
            (ignore-errors
              (destructuring-bind (_input range-min range-max freq)
                  (s-match (rx (group-n 1 (+ num))
                               (* space)
                               (? "-" (* space) (group-n 2 (+ num)))
                               (* space)
                               (group-n 3 bow (or "s" "h" "d" "w" "m" "y")))
                           str)
                (concat ".+"
                        range-min freq
                        (when range-max (concat "/" range-max freq))))))))

    (defun cb-org:read-habit-frequency ()
      "Prompt for a frequency for org-habit."
      (let ((str (read-string "Repeat every: " nil t)))
        (concat (format-time-string "%Y-%m-%d %a ")
                (or (cb-org:time-freq->range-fmt str)
                    (user-error
                     (s-join "\n"
                             '("Unrecognised time specification: %s\n"
                               "Examples:"
                               "  daily"
                               "  every 2 days"
                               "  every 3-4 months"))
                     str)))))

    (defun cb-org:validate-habit (habit)
      (with-temp-buffer
        (org-mode)
        (save-excursion (insert habit))
        (org-habit-parse-todo)))

    (defun cb-org:read-habit ()
      "Read info from the user to construct a new habit."
      (save-window-excursion
        (let* ((desc (s-trim (read-string "Description: " nil t)))
               (freq (cb-org:read-habit-frequency))
               (end (and (ido-yes-or-no-p "Set an end time? ")
                         (org-read-date)))
               (habit (concat
                       "* TODO " desc "\n"
                       "  SCHEDULED: <" freq ">\n"
                       "  :PROPERTIES:\n"
                       "  :STYLE: habit\n"
                       (when end
                         (format "  :LAST_REPEAT: [%s]\n" end))
                       "  :END:")))
          (when (cb-org:validate-habit habit)
            habit))))

;;;; Capture templates

    ;; Insert task file headers.
    (hook-fn 'org-capture-after-finalize-hook
      (when org-last-project-task-file
        (with-current-buffer (find-file-noselect org-last-project-task-file)
          (when (cb-org:format-project-task-file)
            (org-forward-move-past-headers)))))

    (defmacro prev-str-val (sym)
      "Evaluate SYM in the previous active buffer."
      `(or (ignore-errors
             (with-previous-buffer
              ,sym))
           ""))

    ;; Enter insertion mode in capture buffer.
    (hook-fn 'org-capture-mode-hook
      (when (fboundp 'evil-append-line)
        (evil-append-line 1)))

    (add-hook 'org-capture-before-finalize-hook 'indent-buffer 'append)


    (setq org-capture-templates
          `(("T" "Task" entry
             (file+headline (cb-org:project-task-file) "Todos")
             (function cb-org:read-project-task)
             :immediate-finish t
             :empty-lines 1)

            ("t" "Todo" entry
             (file+headline org-default-notes-file "Tasks")
             (function cb-org:read-todo)
             :immediate-finish t
             :empty-lines 1)

            ("d" "Diary" entry
             (file+datetree org-agenda-diary-file)
             (function cb-org:read-diary-entry)
             :immediate-finish t)

            ("h" "Habit" entry
             (file+headline org-default-notes-file "Habits")
             (function cb-org:read-habit)
             :immediate-finish t
             :empty-lines 1)

            ("r" "Reading" entry
             (file+headline org-default-notes-file "Readings")
             "* %^{Title}"
             :immediate-finish t)

            ("l" "Link" entry
             (file+headline org-default-notes-file "Links")
             "* %(prev-str-val w3m-buffer-title)%^{Description}\nl %url"
             :immediate-finish t)

            ("n" "Note" item
             (file+headline org-default-notes-file "Notes")
             "- %?\n"
             :prepend t)))))

(use-package org-clock
  :defer t
  :init (after 'org (require 'org-clock))
  :config
  (progn
    (org-clock-persistence-insinuate)
    (setq org-clock-history-length 20
          org-clock-in-resume t
          org-clock-into-drawer t
          org-clock-remove-zero-time-clocks t
          org-clock-persist t
          org-clock-persist-query-resume nil
          org-clock-report-include-clocking-task t)

    ;; Remove empty LOGBOOK drawers when clocking out.
    (hook-fn 'org-clock-out-hook
      :append t
      (save-excursion
        (beginning-of-line 0)
        (org-remove-empty-drawer-at "LOGBOOK" (point))))))

(use-package org-agenda
  :commands (org-agenda org-agenda-list org-agenda-redo)
  :init
  (progn
    (defvar org-my-archive-expiry-days 2
      "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")

    (when (true? cb:use-vim-keybindings?)
      (bind-key "M-C" 'org-agenda))

    (when (or (daemonp) (display-graphic-p))
      (hook-fn 'after-init-hook
        (executor:org-agenda-fullscreen))))

  :config
  (progn

    (setq org-agenda-files (list org-default-notes-file org-agenda-diary-file)
          org-agenda-insert-diary-extract-time t
          org-agenda-span 'week
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-prewarning-if-scheduled t
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

    ;;;; Agenda refresh

    (defun cb-org:refresh-agenda ()
      "Refresh all org agenda buffers."
      (--each (--filter-buffers (derived-mode-p 'org-agenda-mode))
        (with-current-buffer it
          (org-agenda-redo t))))

    (hook-fn 'org-capture-after-finalize-hook
      (cb-org:refresh-agenda))

    (hook-fn 'org-mode-hook
      (add-hook 'after-save-hook 'cb-org:refresh-agenda nil 'local))

    ;;;; Archiving

    (defun org-archive-done-tasks ()
      (interactive)
      (atomic-change-group
        (org-map-entries 'org-archive-subtree "/DONE" 'file)))

    (defalias 'archive-done-tasks 'org-archive-done-tasks)))

(use-package org-crypt
  :defer t
  :init
  (after 'org
    (require 'org-crypt)
    (org-crypt-use-before-save-magic))
  :config
  (progn
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
        t))))

(use-package appt
  :defer t
  :init (after 'org (require 'appt))
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

(provide 'cb-org)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-org.el ends here
