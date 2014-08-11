;;; config-recentf.el --- Configuration for recentf

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

;; Configuration for recentf

;;; Code:

(require 'recentf)
(require 'utils-common)

(custom-set-variables
 '(recentf-save-file (f-join cb:tmp-dir "recentf"))
 '(recentf-max-saved-items 50)
 '(recentf-max-menu-items 10)
 '(recentf-keep '(file-remote-p file-readable-p))
 '(recentf-exclude
   '("\\.elc$"
     "TAGS"
     "\\.gz$"
     "#$"
     "/elpa/"
     "/tmp/"
     "/temp/"
     ".emacs.d/url/"
     "/\\.git/"
     "/Emacs.app/"
     "/var/folders/"
     "^/?sudo"
     "\\.bbdb"
     "\\.newsrc"
     "/gnus$"
     "/gnus.eld$"
     "\\.ido\\.last"
     "\\.org-clock-save\\.el$")))

(defadvice recentf-cleanup (around hide-messages activate)
  "Do not message when cleaning up recentf list."
  (noflet ((message (&rest args))) ad-do-it))

(recentf-mode +1)

(provide 'config-recentf)

;;; config-recentf.el ends here
