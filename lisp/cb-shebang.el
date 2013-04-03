;;; cb-shebang --- Insert shebang for current mode.

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

;; Insert shebang for current mode.

;;; Code:


(defconst cb:extension->cmd (make-hash-table :test 'equal))
(puthash "py" "python" cb:extension->cmd)
(puthash "sh" "bash"   cb:extension->cmd)
(puthash "rb" "ruby"   cb:extension->cmd)
(puthash "el" "emacs"  cb:extension->cmd)

(defun cb:bufname->cmd (name)
  (gethash (car-safe (last (split-string name "[.]" t)))
           cb:extension->cmd))

(defun insert-shebang ()
  "Insert a shebang line at the top of the current buffer."
  (interactive)
  (let* ((env (shell-command-to-string "where env"))
         (env (replace-regexp-in-string "[\r\n]*" "" env))
         (cmd (cb:bufname->cmd buffer-file-name)))
    (save-excursion
      (goto-char (point-min))
      (insert (concat "#!" env " " cmd))
      (newline 2))))

(provide 'cb-shebang)

;;; cb-shebang.el ends here
