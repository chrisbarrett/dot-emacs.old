;;; config-org-clock.el --- Configuration for org-clock

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

;; Configuration for org-clock

;;; Code:

(require 'org-clock)

(unless noninteractive
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate))

(custom-set-variables
 '(org-clock-out-when-done t)
 '(org-clock-persist-query-resume nil)
 '(org-clock-into-drawer t)
 '(org-clock-history-length 20)
 '(org-clock-in-resume t)
 '(org-clock-auto-clock-resolution 'when-no-clock-is-running)
 '(org-clock-report-include-clocking-task t)
 '(org-clock-in-switch-to-state 'cb-org:clock-in-to-next-state)
 '(org-clock-out-remove-zero-time-clocks t))

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

(provide 'config-org-clock)

;;; config-org-clock.el ends here
