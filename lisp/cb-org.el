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

(require 'dash)
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
          org-default-notes-file (concat org-directory "notes.org"))

    (after 'evil
      (evil-define-key 'normal org-mode-map (kbd "M-P") 'outline-previous-visible-heading)
      (evil-define-key 'normal org-mode-map (kbd "M-N") 'outline-next-visible-heading))

    (hook-fn 'cb:org-minor-modes-hook
      "Diminish org minor modes."
      (--each cb:org-minor-modes
        (ignore-errors (diminish it))))

    (declare-modal-executor org-notes
      :command (find-file org-default-notes-file)
      :bind    "M-O")

    (when (or (daemonp) (display-graphic-p))
      (setq initial-buffer-choice org-default-notes-file)))

  :config
  (progn

    (defun project-task-file ()
      (let ((proj (or (ignore-errors (projectile-project-root))
                      (with-current-buffer (--first-buffer (projectile-project-p))
                        (projectile-project-root)))))
        (concat proj "Tasks.org")))

    (defun cb-org:show-tasks (&optional arg)
      "Display the Tasks tree in the `project-task-file'.
With prefix argument ARG, show the file and move to the tasks tree."
      (interactive "P")
      (let ((task-file
             (or (--first-buffer (equal (buffer-file-name) (project-task-file)))
                 (find-file-noselect (project-task-file)))))
        (if arg
            (switch-to-buffer task-file)
          (with-current-buffer task-file
            (if (emr-blank? (buffer-string))
                (user-error "No current tasks")
              (font-lock-fontify-buffer)
              (message (s-trim (buffer-string))))))))

    (bind-key* "M-?" 'cb-org:show-tasks)

    (setq org-catch-invisible-edits 'smart
          org-pretty-entities t)

    (--each '("NOTES" "COMMENTS")
      (add-to-list 'org-drawers it))

    (hook-fn 'org-mode-hook
      (auto-revert-mode +1)
      (unless (buffer-file-name)
        (cb:append-buffer)))

    (define-key org-mode-map (kbd "M-p") 'org-metaup)
    (define-key org-mode-map (kbd "M-n") 'org-metadown)))

(use-package org-capture
  :commands (org-capture)
  :init
  (progn
    (when cb:use-vim-keybindings?
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

    (defun cb:sort-todos-by-priority ()
      "Sort the Tasks list in the notes file."
      (ignore-errors
        (with-current-buffer (find-file-noselect org-default-notes-file)
          (save-excursion
            (goto-char (point-min))
            (search-forward-regexp (rx bol "*" (+ space) "Tasks" (* space) eol) nil t)
            (cb:sort-tasks-in-subtree)))))

    (add-hook 'org-capture-after-finalize-hook 'cb:sort-todos-by-priority)

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
                        range-min
                        (when range-max (concat "/" range-max))
                        freq))))))

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

    (defun cb-org:read-habit ()
      (let ((desc (s-trim (read-string "Description: " nil t)))
            (freq (cb-org:read-habit-frequency))
            (end (and (ido-yes-or-no-p "Set an end time? ")
                      (org-read-date)))
            (newline "\n"))
        (apply 'concat
               `("* TODO " ,desc ,newline
                 "  SCHEDULED: <" ,freq ">" ,newline
                 "  :PROPERTIES:" ,newline
                 "  :STYLE: habit" ,newline
                 ,@(when end
                     (list (format "  :LAST_REPEAT: [%s]" end)
                           newline))
                 "  :END:"))))

    ;;;; Capture templates

    (defmacro prev-str-val (sym)
      "Evaluate SYM in the previous active buffer."
      `(or (ignore-errors
             (with-previous-buffer
              ,sym))
           ""))

    (setq org-capture-templates
          `(("t" "Task" entry
             (file+headline (project-task-file) "Tasks")
             "* TODO %^{Description}"
             :immediate-finish t)

            ("T" "Todo" entry
             (file+headline org-default-notes-file "Tasks")
             "* TODO [#%^{Priority}] %^{Description}"
             :immediate-finish t)

            ("h" "Habit" entry
             (file+headline org-default-notes-file "Habits")
             (function cb-org:read-habit)
             :empty-lines 1
             :immediate-finish t)

            ("r" "Reading List" entry
             (file+headline org-default-notes-file "Reading List")
             "* %^{Title}"
             :immediate-finish t)

            ("l" "Link" entry
             (file+headline org-default-notes-file "Links")
             "* %(prev-str-val w3m-buffer-title)%^{Description}\n %(prev-str-val w3m-current-url)"
             :immediate-finish t)

            ("n" "Note" item
             (file+headline org-default-notes-file "Notes")
             "- %^{Note}"
             :immediate-finish t))

          org-todo-keywords
          '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))))

(use-package org-agenda
  :commands (org-agenda)
  :init
  (when cb:use-vim-keybindings?
    (bind-key "M-C" 'org-agenda)))

(provide 'cb-org)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-org.el ends here
