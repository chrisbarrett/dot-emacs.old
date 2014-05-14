;;; config-makefiles.el --- Configure makefiles

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

;; Configure makefiles

;;; Code:

(require 'utils-common)

(defun convert-leading-spaces-to-tabs ()
  "Convert sequences of spaces at the beginning of a line to tabs."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (rx bol (group (>= 4 space))) nil t)
      (replace-match "\t"))))

(hook-fn 'makefile-mode-hook
  (setq indent-tabs-mode t)
  (add-hook 'before-save-hook 'convert-leading-spaces-to-tabs nil t))

(after 'make-mode
  (define-key makefile-mode-map (kbd "C-c C-c") nil))

(provide 'config-makefiles)

;;; config-makefiles.el ends here
