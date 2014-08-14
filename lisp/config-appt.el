;;; config-appt.el --- Configuration for appt

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

;; Configuration for appt

;;; Code:

(require 'utils-common)
(require 'appt)
(require 'platform-darwin)

(custom-set-variables
 '(appt-message-warning-time 60)
 '(appt-display-interval 15))

(save-window-excursion
  (appt-activate +1))

(defvar cb-org:appt-update-timer
  (run-with-idle-timer 240 t 'org-agenda-to-appt t))

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
  )

(provide 'config-appt)

;;; config-appt.el ends here
