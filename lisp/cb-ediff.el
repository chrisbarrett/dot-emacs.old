;;; cb-ediff --- Configure Ediff

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Configure Emacs as mergetool.
;; See http://stackoverflow.com/questions/1817370/using-ediff-as-git-mergetool

;;; Code:

(require 'ediff)

(defun cb:apply-diff ()
  (let ((file ediff-merge-store-file))
    (set-buffer ediff-buffer-C)
    (write-region (point-min) (point-max) file)
    (message "Merge buffer saved in: %s" file)
    (set-buffer-modified-p nil)
    (sit-for 1)))

(defun cb:handle-git-merge (local remote base merged)
  "Configure this emacs session for use as the git mergetool."
  (add-hook 'ediff-quit-hook 'kill-emacs)
  (add-hook 'ediff-quit-merge-hook 'cb:apply-diff)
  (ediff-merge-files-with-ancestor local remote base nil merged))

(provide 'cb-ediff)

;;; cb-ediff.el ends here
