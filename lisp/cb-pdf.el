;;; cb-pdf.el --- Commands for working with PDFs.

;; Copyright (C) 2013 Chris Barrett

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

;; Commands for working with PDFs.

;;; Code:

(require 'cb-lib)
(require 'cb-file-picker-widget)

(defun pdf-combine ()
  "Concatenate the given PDF files."
  (interactive)
  (file-picker
   "*Select PDFs*"
   :on-accept
   (lambda (pdfs)
     (let ((dest (read-file-name "Destination: " "~/" nil nil ".pdf")))
       (when (zerop (pdf-combine-command dest pdfs))
         (kill-new dest)
         (message "PDF path copied to kill-ring."))))))

(defun pdf-combine-command (destination pdfs)
  "Concatenate the given PDF files and output to DESTINATION."
  (%-sh (format "pdftk %s cat output %s"
                (s-join " " (-map (C %-quote f-expand) pdfs))
                (%-quote (f-expand destination)))))

(provide 'cb-pdf)

;;; cb-pdf.el ends here
