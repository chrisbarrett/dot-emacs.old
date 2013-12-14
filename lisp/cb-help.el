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

(define-prefix-command 'help-find-map)
(bind-keys
  "C-h e"   'help-find-map
  "C-h e e" 'view-echo-area-messages
  "C-h e f" 'find-function
  "C-h e k" 'find-function-on-key
  "C-h e l" 'find-library
  "C-h e p" 'find-library
  "C-h e v" 'find-variable
  "C-h e a" 'apropos
  "C-h e V" 'apropos-value)

(provide 'cb-help)

;;; cb-help.el ends here
