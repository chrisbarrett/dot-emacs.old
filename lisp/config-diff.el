;;; config-diff.el --- Configuration for diff-mode

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

;; Configuration for diff-mode

;;; Code:

(require 'utils-common)
(require 'config-evil)

(setq diff-switches "-u"
      ediff-window-setup-function 'ediff-setup-windows-plain)

(defun cb-diff:close ()
  (interactive)
  (when (> (length (window-list)) 1)
    (kill-buffer-and-window)))

(after 'diff-mode
  (define-key diff-mode-map (kbd "q") 'cb-diff:close))

(after '(diff-mode evil)
  (add-hook 'ediff-startup-hook 'turn-off-evil-mode)
  (evil-define-key 'normal diff-mode-map (kbd "q") 'cb-diff:close))

(provide 'config-diff)

;;; config-diff.el ends here
