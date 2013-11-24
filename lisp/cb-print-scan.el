;;; cb-print-scan.el --- Commands for working with scanners and printers.

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

;; Commands for working with scanners and printers.

;;; Code:

(require 'cb-lib)

(defun scan (colour-mode destination)
  "Scan using the default scanner to a PDF file.
* COLOUR-MODE should be either \"colour\" or \"grayscale\".
* The document will be created at DESTINATION."
  (interactive
   (list
    (ido-completing-read "Mode: " '("Colour" "Grayscale"))
    (read-file-name "Destination: " "~/" nil nil ".pdf")))

  ;; Scan as a TIFF to a temp file, then export to PDF with CUPS.
  (let ((tmpfile (make-temp-file "scan--" nil ".tiff")))
    (%-async (concat "scanimage"
                     " --mode=" colour-mode
                     " --format=tiff"
                     " > " tmpfile
                     " && cupsfilter -D -i image/tiff " tmpfile
                     " > " (%-quote (f-expand destination)))))

  (kill-new destination)
  (message "Scan started. Destination path copied to kill-ring."))

(define-command-picker printer-scanner-picker
  :title
  "*Print/Scan*"
  :options
  '(("s" "Scan" scan)
    ("p" "Print Buffer" print-buffer)
    ("r" "Print Region" print-region)))

(bind-key* "<f10>" 'printer-scanner-picker)

(provide 'cb-print-scan)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-print-scan.el ends here
