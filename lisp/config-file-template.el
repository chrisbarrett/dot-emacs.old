;;; config-file-template.el --- Configure file templates

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

;; Configure file templates

;;; Code:

(require 'utils-common)
(require 'file-template)

(add-hook 'find-file-not-found-hooks
          'file-template-find-file-not-found-hook 'append)

(setq file-template-insert-automatically t)

(defvar cb:file-templates-dir (f-join user-emacs-directory "templates"))
(setq file-template-paths (list cb:file-templates-dir))

(setq file-template-mapping-alist
      (->> (f-files cb:file-templates-dir)
        (-map 'f-filename)
        (--map (cons (format "\\.%s$" (f-ext it)) it))))

(hook-fn 'file-template-insert-hook
  (setq buffer-undo-list nil
        buffer-undo-tree nil))

(defun cbtmpl:org-skeleton-title (filename)
  "Format the title to use for the given FILENAME."
  (->> (f-filename (f-no-ext filename))
    s-split-words
    (-map 's-capitalize)
    (s-join " ")))

(provide 'config-file-template)

;;; config-file-template.el ends here
