;;; cb-progress-bar-widget.el --- Display progress bars in Emacs

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris@chris-macbook.lan>
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

;; Display progress bars in Emacs

;;; Code:

(require 'noflet)
(require 'dash)
(require 's)

(cl-defun format-progress-bar (title pos length)
  "Format a progress bar with TITLE and pips up to POS along its LENGTH.
POS should be a number between 1 and LENGTH."
  (cl-assert (cl-plusp pos))
  (cl-assert (cl-plusp length))
  (let* (
         ;; Format title and percentage.
         (leader (concat title " "))
         (percentage (round (* 100 (/ (float pos) length))))
         (trailer (concat " " (number-to-string percentage) "%"))
         ;; Compute the max length of the bar, including the leader and title.
         (max-len (- (1- (window-width))
                     (+ (length leader) (length trailer))))
         ;; Constrain the progress meter to fit within MAX-LEN columns.
         (scaled-length (round (min max-len length)))
         (step-scale (/ scaled-length length))
         (scaled-pos (round (* step-scale pos))))
    (concat leader
            (s-repeat scaled-pos ".")
            (s-repeat (- scaled-length scaled-pos) " ")
            trailer)))

(cl-defun run-with-progress-bar
    (title actions &key (silent? t) &allow-other-keys)
  "Call ACTIONS, printing a progress bar with TITLE.

By default, calls to `message' in each action will be suppressed.
Use the SILENT? keyword to explicitly override this behaviour.

In batch mode, this just prints a summary instead of progress."
  (cl-assert (stringp title))
  (cl-assert (sequencep actions))
  (cl-loop
   when actions

   initially (when noninteractive (message "%s..." title))
   finally   (when noninteractive (message "%s...done" title))

   ;; Get the step-number and action.
   with len = (length actions)
   for (i . action) in (--map-indexed (cons (1+ it-index) it) actions)
   ;; Run action.
   do (if silent?
          (noflet ((message (&rest _) (ignore))) (funcall action))
        (funcall action))
   ;; Print progress bar. Do not add it to the *Messages* buffer.
   do (unless noninteractive
        (let ((message-log-max nil))
          (message "%s" (format-progress-bar title i len))))))


(provide 'cb-progress-bar-widget)

;;; cb-progress-bar-widget.el ends here
