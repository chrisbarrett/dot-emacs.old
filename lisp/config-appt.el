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

(custom-set-variables
 '(appt-message-warning-time 60)
 '(appt-display-interval 15))

(save-window-excursion
  (appt-activate +1))

(defun cb-org:check-appt ()
  "Synchronise org appointments with appt the next time Emacs is idle."
  (run-with-idle-timer 5 nil (lambda ()
                               (org-agenda-to-appt t)
                               (appt-check))))

(defvar cb-org:appt-update-timer
  (run-with-timer 240 t 'cb-org:check-appt))

(provide 'config-appt)

;;; config-appt.el ends here
