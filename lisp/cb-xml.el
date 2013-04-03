;;; cb-xml --- Functions for working with XML files.

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

;; Functions for working with XML files.

;;; Code:

(require 'cl-lib)

(defun cb:reformat-xml-in-region (begin end)
  (save-excursion
    (let ((end end))
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char)
        (insert "\n")
        (cl-incf end))
      (indent-region begin end))))

(defun cb:reformat-xml ()
  "Insert newlines and indent XML. Operates on region, or the whole buffer if no region is defined."
  (interactive)
  (save-excursion
    (let ((start (or (ignore-errors (region-beginning))
                     (point-min)))
          (end   (or (ignore-errors (region-end))
                     (point-max))))
      (cb:reformat-xml-in-region start end))))

(provide 'cb-xml)

;;; cb-xml.el ends here
