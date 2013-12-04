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

(use-package recentf
  :config
  (progn

    (add-hook 'kill-emacs-hook 'recentf-save-list)
    (run-with-idle-timer (* 5 60) t 'recentf-save-list)

    (defadvice recentf-cleanup (around hide-messages activate)
      "Suppress messages when cleaning up recentf."
      (noflet ((message (&rest args)))
        ad-do-it))

    (setq
     recentf-save-file       (concat cb:tmp-dir "recentf")
     recentf-auto-cleanup    5
     recentf-keep            '(file-remote-p file-readable-p)
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
                       ;; Special files
                       "\\.bbdb"
                       "\\.newsrc"
                       "^/?sudo"
                       "recentf"
                       "/gnus$"
                       "/gnus.eld$"
                       "\\.ido\\.last"
                       ))

    ;; Sometimes recentf gets into a recursive load, so just nuke the save file
    ;; if that happens.
    (condition-case _
        (recentf-mode +1)
      (error
       (f-delete recentf-save-file)
       (recentf-mode +1)))))

(provide 'cb-recentf)

;; Local Variables:
;; End:

;;; cb-recentf.el ends here
