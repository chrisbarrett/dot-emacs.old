;;; config-fortune.el --- Configuration for fortune

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

;; Configuration for fortune

;;; Code:

(require 'utils-common)

(defun fortune ()
  "Display a quotation from the 'fortune' program."
  (interactive)
  (-when-let (fortune (--first (ignore-errors (file-exists-p it))
                               (list (executable-find "fortune")
                                     "/usr/bin/fortune"
                                     "/usr/local/bin/fortune" )))
    (message (s-trim (shell-command-to-string (concat fortune " -s -n 250"))))))

(defun cb:show-fortune (&rest _)
  "Show fortune if started without a file to visit."
  (run-with-idle-timer
   0.1 nil
   (lambda ()
     (when (or (-contains? '("*scratch*" "notes.org" "todo.org") (buffer-name))
               (derived-mode-p 'org-agenda-mode))
       (fortune)))))

(add-hook 'after-make-frame-functions 'cb:show-fortune)
(add-hook 'after-init-hook 'cb:show-fortune)


(provide 'config-fortune)

;;; config-fortune.el ends here
