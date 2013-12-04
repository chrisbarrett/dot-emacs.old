;;; cb-recentf.el --- Configuration for recentf

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130914.0959

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

(require 'use-package)
(require 'cb-lib)
(require 'noflet)
(require 'cb-paths)

;; `recentf' adds a recent files list.
(use-package recentf
  :init
  (setq recentf-save-file (f-join cb:tmp-dir "recentf"))
  :config
  (progn

    (defadvice recentf-cleanup (around hide-messages activate)
      "Suppress messages when cleaning up recentf."
      (noflet ((message (&rest args)))
        ad-do-it))

    (setq
     recentf-keep '(file-remote-p file-readable-p)
     recentf-max-saved-items 100
     recentf-max-menu-items  25
     recentf-exclude '(
                       ;; Filetypes
                       "\\.elc$"
                       "TAGS"
                       "\\.gz$"
                       "#$"
                       ;; Special directories
                       "/elpa/"
                       "/tmp/"
                       "/temp/"
                       ".emacs.d/url/"
                       "/\\.git/"
                       "/Emacs.app/"
                       ;; Tramp
                       "^/?sudo"
                       ;; Special files
                       "\\.bbdb"
                       "\\.newsrc"
                       "/gnus$"
                       "/gnus.eld$"
                       "\\.ido\\.last"
                       "\\.org-clock-save\\.el$"
                       ))

      (recentf-mode)))

(provide 'cb-recentf)

;; Local Variables:
;; End:

;;; cb-recentf.el ends here
