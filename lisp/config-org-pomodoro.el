;;; config-org-pomodoro.el --- Configure org-pomodoro

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

;; Configure org-pomodoro

;;; Code:

(require 'utils-common)
(require 'org-pomodoro)
(require 'config-darwin)

(bind-key* "<f5>" 'org-pomodoro)

(custom-set-variables
 '(org-pomodoro-long-break-length 25)
 '(org-pomodoro-format "• %s")
 '(org-pomodoro-short-break-format "Break %s")
 '(org-pomodoro-long-break-format "Break %s")
 '(org-pomodoro-show-seconds nil)
 '(org-pomodoro-show-in-mode-line nil))


(when (equal system-type 'darwin)
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

(provide 'config-org-pomodoro)

;;; config-org-pomodoro.el ends here
