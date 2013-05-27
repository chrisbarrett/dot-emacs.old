;;; cb-python.el --- Configuration for python

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130526.2358

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

;; Configuration for python

;;; Code:

(require 'use-package)

(use-package python
  :ensure   t
  :commands python-mode
  :mode     ("\\.py$" . python-mode)
  :config
  (progn
    (defun cb:comma-then-space ()
      (interactive)
      (atomic-change-group
        (insert-char ?\,)
        (just-one-space)))

    (defun cb:switch-to-python ()
      "Switch to the last active Python buffer."
      (interactive)
      (-when-let (buf (last-buffer-for-mode 'python-mode))
        (pop-to-buffer buf)))

    (define-key python-mode-map (kbd ",") 'cb:comma-then-space)
    (define-key inferior-python-mode-map (kbd ",") 'cb:comma-then-space)
    (define-key inferior-python-mode-map (kbd "C-c C-z") 'cb:switch-to-python)
    (add-to-list 'ac-modes 'python-mode)
    (add-to-list 'ac-modes 'inferior-python-mode)))

(use-package jedi
  :ensure   t
  :commands jedi:setup
  :init     (add-hook 'cb:python-modes-hook 'jedi:setup)
  :config   (setq jedi:setup-keys t))

(provide 'cb-python)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-python.el ends here
