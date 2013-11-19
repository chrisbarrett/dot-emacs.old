;;; cb-backups.el --- Configuration for backups and session state

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0021

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

;; Configuration for backups and session state

;;; Code:

(require 'use-package)
(require 'noflet)

(use-package saveplace
  :defer t
  :commands
  (save-place-find-file-hook
   save-place-kill-emacs-hook
   save-place-to-alist)
  :init
  (progn
    (unless noninteractive
      (add-hook 'kill-emacs-hook 'save-place-kill-emacs-hook))
    (add-hook 'kill-buffer-hook  'save-place-to-alist)
    (add-hook 'find-file-hook    'save-place-find-file-hook t)
    (add-hook 'server-visit-hook 'save-place-find-file-hook)
    (add-hook 'server-done-hook  'save-place-kill-emacs-hook)

    (setq save-place-file (concat cb:tmp-dir "saved-places"))
    (setq-default save-place t)))

(use-package backup-dir
  :defer t
  :idle  (require 'backup-dir)
  :init
  (hook-fn 'find-file-hook (require 'backup-dir))
  :config
  (setq auto-save-file-name-transforms `((".*" ,(concat cb:autosaves-dir "\\1") t))
        backup-by-copying        t
        bkup-backup-directory-info `((".*" ,cb:backups-dir ok-create))
        auto-save-list-file-name (concat cb:autosaves-dir "autosave-list")
        delete-old-versions      t
        kept-new-versions        6
        kept-old-versions        2
        version-control          t))

(use-package savehist
  :config
  (progn
    (setq
     savehist-additional-variables '(search ring regexp-search-ring)
     savehist-autosave-interval    60
     savehist-file                 (concat cb:tmp-dir "savehist"))
    ;; Load savehist. Delete the savehist file if there is a problem. This
    ;; can happen with tramp, for example.
    (condition-case _
        (savehist-mode +1)
      (void-variable
       (delete-file savehist-file)
       (savehist-mode +1)))))

(provide 'cb-backups)

;; Local Variables:
;; End:

;;; cb-backups.el ends here
