;;; config-org-agenda.el --- Configure org-agenda

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

;; Configure org-agenda

;;; Code:

(require 'utils-common)
(require 'org-agenda)

(custom-set-variables
 '(org-agenda-auto-exclude-function 'cb-org:exclude-tasks-on-hold)
 '(org-agenda-diary-file (f-join org-directory "diary.org"))
 '(org-agenda-hide-tags-regexp (rx (or "noexport" "someday")))
 '(org-agenda-insert-diary-extract-time t)
 '(org-agenda-ndays 7)
 '(org-agenda-search-view-always-boolean t)
 '(org-agenda-show-all-dates nil)
 '(org-agenda-show-inherited-tags nil)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-keep)
     (todo priority-down category-keep scheduled-up)
     (tags priority-down category-keep)
     (search category-keep)))
 '(org-agenda-span 'week)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-text-search-extra-files '(agenda-archives))

 '(org-agenda-custom-commands
   (->> '(("A" "Agenda and next actions"
           ((tags-todo "-someday-media/NEXT"
                       ((org-agenda-overriding-header "Next Actions")))
            (agenda "")
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting")))
            (tags-todo "media/NEXT"
                       ((org-agenda-overriding-header "Media"))))
           ((org-agenda-tag-filter-preset
             '("-@work"))))
          ("w" "Agenda and work actions"
           ((tags-todo "-someday/NEXT"
                       ((org-agenda-overriding-header "Next Actions")))
            (agenda ""
                    ((org-agenda-ndays 14)))
            (todo "AWAITING|ORGANISE_IN"
                  ((org-agenda-overriding-header "Incoming")))
            (todo "TODO_OUT|READY|ORGANISE_OUT"
                  ((org-agenda-overriding-header "Outgoing")))
            (tags-todo "-someday/WAITING"
                       ((org-agenda-overriding-header "Waiting"))))
           ((org-agenda-tag-filter-preset
             '("+@work"))
            (org-agenda-files
             (list org-work-file))
            (org-agenda-hide-tags-regexp
             (regexp-opt
              (list org-agenda-hide-tags-regexp "@work")))))
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
          ("ge" "Errands" tags-todo "@errand")
          ("gp" "Phone" tags-todo "@phone")
          ("gw" "Work" tags-todo "@work")
          ("gh" "Home" tags-todo "@home")
          ("gl" "Leisure" tags-todo "@leisure")
          ("r" "Weekly Review"
           ((agenda ""
                    ((org-agenda-ndays 14)))
            (stuck "")
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting")))
            (tags-todo "someday-skill/MAYBE|NEXT"
                       ((org-agenda-overriding-header "Someday")))
            (tags-todo "someday&skill"
                       ((org-agenda-overriding-header "Skills")))
            (tags-todo "media"
                       ((org-agenda-overriding-header "Media"))))
           ((org-agenda-tag-filter-preset
             '("-drill" "-gtd"))
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
          ("ge" "Errands" tags-todo "@errand")
          ("gp" "Phone" tags-todo "@phone")
          ("gw" "Work" tags-todo "@work")
          ("gh" "Home" tags-todo "@home")
          ("gl" "Leisure" tags-todo "@leisure"))
     (--map-when
      (listp
       (cdr it))
      (append it
              '(((org-agenda-customise-window-hook 'delete-other-windows))))))))

(add-hook 'org-agenda-mode-hook 'org-agenda-to-appt)

(add-to-list 'org-agenda-files org-directory)

(defvar org-agenda-customise-window-hook nil
  "Relay hook for `org-agenda-mode-hook'.  Suitable for setting up the window.")

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

(when (or (daemonp) (display-graphic-p))
  (hook-fn 'after-init-hook
    (unless noninteractive
      (executor:org-agenda-fullscreen))))


(provide 'config-org-agenda)

;;; config-org-agenda.el ends here
