;;; cb-help.el --- Configuration for help-mode and related functionality

;; Copyright (C) 2013 Chris Barrett

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

;; Configuration for help-mode and related functionality

;;; Code:

(require 'cb-lib)

(define-command-picker help-picker
  :title "*Help Commands*"
  :options
  `(("m" "Messages" view-echo-area-messages)
    ("f" "Find Function" find-function)
    ("l" "Find Library" find-library)
    ("v" "Find Variable" find-variable)
    ("a" "Apropos" apropos)
    ("A" "Apropos (value)" apropos-value)))

(bind-key "C-h e" 'help-picker)

(provide 'cb-help)

;;; cb-help.el ends here
