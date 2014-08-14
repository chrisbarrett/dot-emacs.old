;;; config-ido.el --- Configuration for ido

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

;; Configuration for ido

;;; Code:

(require 'utils-common)

(require 'ido)
(ido-mode +1)
(cb:install-package 'ido-hacks t)
(cb:install-package 'flx-ido t)
(ido-hacks-mode +1)
(flx-ido-mode +1)
(cb:install-package 'ido-vertical-mode t)
(noflet ((message (&rest _) nil)) (ido-vertical-mode +1))

(custom-set-variables
 '(ido-enable-prefix nil)
 '(ido-save-directory-list-file (f-join cb:tmp-dir "ido.last"))
 '(ido-enable-flex-matching t)
 '(ido-create-new-buffer 'always)
 '(ido-use-filename-at-point 'guess)
 '(ido-max-prospects 10)
 '(ido-default-file-method 'selected-window))

(defmacro declare-ido-wrapper (command)
  "Make COMMAND use ido for file and directory completions."
  `(defadvice ,command (around read-with-ido activate)
     (noflet
       ((read-directory-name
         (&rest args) (apply 'ido-read-directory-name args))
        (read-file-name
         (&rest args) (apply 'ido-read-file-name args))
        (read-buffer
         (&rest args) (apply 'ido-read-buffer)))
       ad-do-it)))

(add-to-list 'ido-ignore-buffers "\\*helm.*")
(add-to-list 'ido-ignore-buffers "\\*Minibuf.*")
(add-to-list 'ido-ignore-files "\\.swp")
(add-to-list 'ido-ignore-files "\\.DS_Store")

;;; Insert spaces literally in ido

(defalias 'ido-complete-space 'self-insert-command)

;;; Key bindings

(bind-keys
  "C-x C-f" 'ido-find-file
  "C-x d"   'ido-dired
  "C-x i"   'ido-insert-file
  "C-x C-w" 'ido-write-file
  "C-x k"   'ido-kill-buffer
  "C-x b"   'ido-switch-buffer)

(provide 'config-ido)

;;; config-ido.el ends here
