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

(use-package org
  :ensure t
  :defer  t
  :idle   (require 'org)
  :init
  (progn
    (setq org-modules '(org-bbdb org-w3m org-habit)
          org-directory (concat user-home-directory "org/")
          org-default-notes-file (concat org-directory "notes.org")
          org-reverse-note-order t)

    (after 'evil
      (evil-define-key 'normal org-mode-map (kbd "M-P") 'outline-previous-visible-heading)
      (evil-define-key 'normal org-mode-map (kbd "M-N") 'outline-next-visible-heading)

      (evil-define-key 'normal org-mode-map (kbd "SPC") 'org-cycle)
      (evil-define-key 'normal org-mode-map (kbd "z m") (command (org-global-cycle 1)))
      (evil-define-key 'normal org-mode-map (kbd "z r") (command (org-global-cycle 0))))

    (hook-fn 'cb:org-minor-modes-hook
      "Diminish org minor modes."
      (--each cb:org-minor-modes
        (ignore-errors (diminish it)))))

  :config
  (progn

    (hook-fn 'kill-emacs-hook
      "Save notes file when exiting emacs."
      (ignore-errors
        (-when-let (buf (--first-buffer (equal buffer-file-name org-default-notes-file)))
          (with-current-buffer buf
            (save-buffer)))))

    ;;;; Tasks

    (defun project-task-file ()
      (let ((proj (or (ignore-errors (projectile-project-root))
                      (with-current-buffer (--first-buffer (projectile-project-p))
                        (projectile-project-root)))))
        (concat proj "Tasks.org")))

    (defun cb-org:show-task-file ()
      (switch-to-buffer
       (or (--first-buffer (equal (buffer-file-name) (project-task-file)))
           (find-file-noselect (project-task-file)))))

    (defun cb-org:task-file-contents ()
      (save-excursion
        (-if-let (buf (--first-buffer (equal (buffer-file-name) (project-task-file))))
          (with-current-buffer buf
            (font-lock-fontify-buffer)
            (buffer-string))
          (with-current-buffer (find-file-noselect (project-task-file) t)
            (prog1 (buffer-string)
              (kill-buffer))))))

    (defun cb-org:show-tasks (&optional arg)
      "Display the Tasks tree in the `project-task-file'.
With prefix argument ARG, show the file and move to the tasks tree."
      (interactive "P")
      (if arg
          (cb-org:show-task-file)
        (let ((str (cb-org:task-file-contents)))
          (if (emr-blank? str)
              (user-error "No current tasks")
            (message (s-trim str))))))

    (bind-key* "M-?" 'cb-org:show-tasks)

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

    (hook-fn 'org-mode-hook
      (auto-revert-mode +1)
      ;; Enter insert mode if this is a capture window or something.
      (unless (buffer-file-name)
        (cb:append-buffer)))

    (define-key org-mode-map (kbd "M-p") 'org-metaup)
    (define-key org-mode-map (kbd "M-n") 'org-metadown)

    (defadvice org-add-log-note (before exit-minibuffer activate)
      "Prevent attempts to expand the minibuffer."
      (when (minibufferp (window-buffer (selected-window)))
        (other-window 1)))))

(use-package org-capture
  :commands (org-capture)
  :init
  (progn
    (when (truthy? 'cb:use-vim-keybindings?)
      (bind-key "M-o" 'org-capture))

    (hook-fn 'org-capture-mode-hook
      (when (fboundp 'evil-insert)
        (evil-insert 0))))
  :config
  (progn

;;;; Todo auto-sorting

    (defun cb:sort-tasks-in-subtree ()
      "Sort child elements of the tree at point."
      (let ((beg (1+ (line-number-at-pos)))
            (end (save-excursion
                   (org-mark-subtree)
                   (region-end))))
        (push-mark beg)
        (push-mark end)
        (org-sort-entries t 112)))

    (defun cb-org:read-todo ()
      "Read a todo item for org-capture."
      (save-window-excursion
        (let ((desc (s-trim (read-string "Description: " nil t)))
              (start (org-read-date)))
          (concat "* TODO " desc "\n"
                  "  SCHEDULED: <" start "> \n"))))

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

    (defmacro prev-str-val (sym)
      "Evaluate SYM in the previous active buffer."
      `(or (ignore-errors
             (with-previous-buffer
              ,sym))
           ""))

    (defun cb-org:capture-todo ()
      "Capture a todo."
      (org-capture nil "t"))

    (global-set-key (kbd "M-t") 'cb-org:capture-todo)

    (setq org-capture-templates
          `(("T" "Task" entry
             (file+headline (project-task-file) "Tasks")
             "* TODO %^{Description}"
             :immediate-finish t)

            ("t" "Todo" entry
             (file+headline org-default-notes-file "Tasks")
             (function cb-org:read-todo)
             :empty-lines 1
             :immediate-finish t)

            ("h" "Habit" entry
             (file+headline org-default-notes-file "Habits")
             (function cb-org:read-habit)
             :empty-lines 1
             :immediate-finish t)

            ("r" "Reading" entry
             (file+headline org-default-notes-file "Readings")
             "* %^{Title}"
             :empty-lines 1
             :immediate-finish t)

            ("l" "Link" entry
             (file+headline org-default-notes-file "Links")
             "* %(prev-str-val w3m-buffer-title)%^{Description}\nl %(prev-str-val w3m-current-url)"
             :immediate-finish t)

            ("n" "Note" item
             (file+headline org-default-notes-file "Notes")
             "- %^{Note}"
             :immediate-finish t)))))

(use-package calendar
  :init
  (progn
    (setq diary-file (concat user-dropbox-directory "diary"))
    (add-to-list 'auto-mode-alist `(,(rx bow "diary" eol) . diary-mode)))
  :config
  (progn

    ;; Create the diary file if it does not exist.
    (unless (f-exists? diary-file)
      (f-write diary-file))

    (defun cb:close-diary ()
      (kill-buffer)
      (when (< 1 (length (window-list))) (delete-window))
      (cb-org:refresh-agenda))

    (defun cb:diary-finalize ()
      "Accept the diary entry and close the diary."
      (interactive)
      (save-buffer)
      (cb:close-diary))

    (defun cb:diary-cancel ()
      "Abort the diary entry and close the diary."
      (interactive)
      (revert-buffer t t)
      (cb:close-diary))

    (hook-fn 'diary-mode-hook
      (add-hook 'after-save-hook 'cb-org:refresh-agenda t 'local)
      (local-set-key (kbd "C-c C-k") 'cb:diary-cancel)
      (local-set-key (kbd "C-c C-c") 'cb:diary-finalize))))

(use-package org-agenda
  :commands (org-agenda org-agenda-list)
  :init
  (progn
    (defvar org-my-archive-expiry-days 2
      "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")

    (declare-modal-view org-agenda-list)

    (declare-modal-executor org-agenda-list
      :bind "M-O"
      :command
      (lambda (&optional arg) (interactive "P")
        (if arg
            (find-file org-default-notes-file)
          (org-agenda-list nil nil 1))))

    (defun cb-org:refresh-agenda ()
      "Refresh all org agenda buffers."
      (--each (--filter-buffers (derived-mode-p 'org-agenda-mode))
        (with-current-buffer it
          (org-agenda-redo t))))

    (when (truthy? 'cb:use-vim-keybindings?)
      (bind-key "M-C" 'org-agenda))

    (when (or (daemonp) (display-graphic-p))
      (hook-fn 'after-init-hook
        (org-agenda-list))))

  :config
  (progn

    (setq org-agenda-files (list org-default-notes-file)
          org-agenda-include-diary t
          org-agenda-span 'week
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-prewarning-if-scheduled t
          ;; Ensure the agenda shows the whole coming week.
          org-agenda-start-on-weekday nil
          org-agenda-ndays 7)

    (define-key org-agenda-mode-map (kbd "g") 'org-agenda-goto-date)
    (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-item)
    (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-item)

    (after 'smartparens
      (hook-fn 'org-agenda-mode-hook
        (smartparens-mode -1)))

    (hook-fn 'org-capture-after-finalize-hook
      (cb-org:refresh-agenda))

    (hook-fn 'org-mode-hook
      (add-hook 'after-save-hook 'cb-org:refresh-agenda nil 'local))

    (defadvice org-agenda-diary-entry (after narrow-and-insert activate)
      "Make the diary entry process similar to org-capture."
      (narrow-to-region (line-beginning-position) (line-end-position))
      (cb:append-buffer)
      ;; Show usage. Run on timer so other messages are buried.
      (run-with-timer
       0.1 nil
       (lambda () (message "<C-c C-c> to commit changes, <C-c C-k> to abort."))))

    ;;;; Archiving

    (defun cb-org:archive-done-tasks ()
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (let ((done-regexp
               (concat "\\* \\(" (regexp-opt org-done-keywords) "\\) "))
              (state-regexp
               (concat "- State \"\\(" (regexp-opt org-done-keywords)
                       "\\)\"\\s-*\\[\\([^]\n]+\\)\\]")))
          (while (re-search-forward done-regexp nil t)
            (let ((end (save-excursion
                         (outline-next-heading)
                         (point)))
                  begin)
              (goto-char (line-beginning-position))
              (setq begin (point))
              (when (re-search-forward state-regexp end t)
                (let* ((time-string (match-string 2))
                       (when-closed (org-parse-time-string time-string)))
                  (if (>= (time-to-number-of-days
                           (time-subtract (current-time)
                                          (apply #'encode-time when-closed)))
                          org-my-archive-expiry-days)
                      (org-archive-subtree)))))))))

    (defalias 'archive-done-tasks 'cb-org:archive-done-tasks)))

(provide 'cb-org)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-org.el ends here
