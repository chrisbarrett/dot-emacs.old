;;; config-org-diary.el --- Configure org diary

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

;; Configure org diary

;;; Code:

(require 'utils-common)
(require 'calendar)
(require 'org)

(custom-set-variables
 '(calendar-date-style 'european))

(defvar date nil
  "Dynamic var bound to current date by calendaring functions.")

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

(defun cb-org:save-diary ()
  (save-restriction
    (save-window-excursion
      (org-agenda-to-appt t)
      (appt-check 'force))))

(hook-fn 'org-mode-hook
  (when (equal (buffer-file-name) org-agenda-diary-file)
    (add-hook 'after-save-hook 'cb-org:save-diary nil t)))

(provide 'config-org-diary)

;;; config-org-diary.el ends here
