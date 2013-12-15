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
(require 'cb-pdf)
(autoload 'org-attach-attach "org-agenda")

(defun cbscan:scan-to-file (colour-mode path)
  "Scan to PATH. Return nil if scanning failed, or PATH if succeeded."
  (when (zerop (%-sh (concat "scanimage"
                             " --mode=" colour-mode
                             " --format=tiff"
                             " > " path)))
    path))

(defun scan-file (colour-mode destination &optional async?)
  "Scan as a TIFF to a temp file, then export to PDF with CUPS.

* COLOUR-MODE should be either \"colour\" or \"grayscale\".

* The document will be created at DESTINATION.

* If ASYNC? is non-nil, the scan will be performed in the background."
  (save-window-excursion
    (let* ((tmpfile (make-temp-file "scan--" nil ".tiff"))
           (command (concat "scanimage"
                            " --mode=" colour-mode
                            " --format=tiff"
                            " > " tmpfile
                            " && cupsfilter -D -i image/tiff " tmpfile
                            " > " (%-quote (f-expand destination)))))
      (cond (async? (%-async command))
            ((zerop (%-sh command))
             destination)))))

(defun scan-batch-to-file (colour-mode destination)
  "Scan several files from the document feeder then export to a single PDF.
The args COLOUR-MODE and DESTINATION are the same as for `scan-file'."
  (interactive
   (list
    (ido-completing-read "Mode: " '("Colour" "Grayscale"))
    (read-file-name "Destination: " "~/" nil nil ".pdf")))

  (read-char-choice "Press <return> to start." (list ?\^M))
  (let (pdfs)
    ;; Scan PDFs in flatbed.
    (save-window-excursion
      (let ((tmpdir (make-temp-file "scan-" t)))

        ;; Scan with document feeder.
        (unless (zerop (%-sh (concat "scanimage"
                                     " --mode=" colour-mode
                                     " --format=tiff"
                                     " --batch=" (f-join tmpdir "scan--%d.tiff"))))
          (error "Scanning failed"))
        (let ((pdfs)))
        ;; Export files to PDFs.
        (unless (->> (f-files tmpdir)

                  (--map (%-sh "cupsfilter -D -i image/tiff" (%-quote it)
                               ">" (%-quote (concat it ".pdf"))))
                  (-all? 'zerop))
          (error "PDF export failed"))
        ;; Combine PDFs.
        (unless (zerop (pdf-combine-command destination (f-files tmpdir)))
          (error "PDF combination failed"))))

    (kill-new destination)
    (message "PDF path copied to kill-ring.")))

(defun scan (colour-mode destination)
  "Scan using the default scanner to a PDF file.

* COLOUR-MODE should be either \"colour\" or \"grayscale\".

* The document will be created at DESTINATION."
  (interactive
   (list
    (ido-completing-read "Mode: " '("Colour" "Grayscale"))
    (read-file-name "Destination: " "~/" nil nil ".pdf")))

  (scan-file colour-mode destination 'async)
  (kill-new destination)
  (message "Scan started. Destination path copied to kill-ring."))

(defun cbscan:read-continue? ()
  "Prompt the user whether to continue scanning."
  (equal ?\^M (read-char-choice "Press <return> to scan or C-d to finish."
                                (list ?\^M ?\^D))))

(defun scan-then-combine (colour-mode destination)
  "Scan several documents on the flatbed, then combine to a single PDF.

* COLOUR-MODE should be either \"colour\" or \"grayscale\".

* The document will be created at DESTINATION."
  (interactive
   (list
    (ido-completing-read "Mode: " '("Colour" "Grayscale"))
    (read-file-name "Destination: " "~/" nil nil ".pdf")))

  (save-window-excursion
    (let ((tmpdir (make-temp-file "scan-" t))
          (n 1))
      ;; Scan PDFs in flatbed.
      (while (cbscan:read-continue?)
        (message "Scanning...")
        (if (cbscan:scan-to-file colour-mode (f-join tmpdir (format "%03i.tiff" n)))
            (cl-incf n)
          (error "Scanning failed")))

      ;; Export files to PDFs.
      (let ((pdfs (-map 'tiff->pdf (f-files tmpdir))))
        (when (-any? 'null pdfs)
          (error "PDF export failed"))

        ;; Combine PDFs.
        (unless (zerop (pdf-combine-command destination pdfs))
          (error "PDF combination failed"))

        (f-delete tmpdir 'force)

        (when (called-interactively-p nil)
          (kill-new destination)
          (message "%s page(s) scanned. PDF path copied to kill-ring." (length pdfs)))

        destination))))

(defun org-attach-from-scanner (colour-mode)
  "Scan a document and attach it to the current org heading.

* COLOUR-MODE is either \"colour\" or \"grayscale\"."
  (interactive (list (ido-completing-read "Mode: " '("Colour" "Grayscale"))))
  (let ((file (scan-then-combine colour-mode (make-temp-file "scan--"))))
    (org-attach-attach file nil 'mv))

  (message "Scanning...Done"))

(define-command-picker printer-scanner-picker
  :title
  "*Print/Scan*"
  :options
  '(("p" "Print Buffer" print-buffer)
    ("r" "Print Region" print-region)
    ("s" "Scan (flatbed)" scan)
    ("m" "Scan Multiple & Combine (flatbed)" scan-then-combine)
    ("u" "Scan (document feeder)" scan-batch-to-file)
    ("a" "Scan and Attach (org)" org-attach-from-scanner
     :when (lambda () (derived-mode-p 'org-mode)))))

(bind-key* "<f10>" 'printer-scanner-picker)

(provide 'cb-print-scan)

;;; cb-print-scan.el ends here
