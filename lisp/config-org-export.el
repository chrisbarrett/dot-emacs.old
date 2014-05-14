;;; config-org-export.el --- Configure org-export

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

;; Configure org-export

;;; Code:

(custom-set-variables
 '(org-export-exclude-tags '("noexport" "crypt"))
 '(org-html-html5-fancy t)
 '(org-html-postamble nil)
 '(org-html-table-row-tags
   (cons
    '(cond
      (top-row-p "<tr class=\"tr-top\">")
      (bottom-row-p "<tr class=\"tr-bottom\">")
      (t
       (if
           (=
            (mod row-number 2)
            1)
           "<tr class=\"tr-odd\">" "<tr class=\"tr-even\">")))
    "</tr>")))


(setq org-html-head-extra
      "
<style type=\"text/css\">
table tr.tr-odd td {
      background-color: #FCF6CF;
}
table tr.tr-even td {
      background-color: #FEFEF2;
}
</style>
")


(require 'ox-texinfo)
(add-to-list 'org-export-snippet-translation-alist
             '("info" . "texinfo"))

(require 'ox-koma-letter)
(add-to-list 'org-latex-classes '("koma-letter" "
\\documentclass[paper=A4,pagesize,fromalign=right,
               fromrule=aftername,fromphone,fromemail,
               version=last]{scrlttr2}
\\usepackage[english]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage[normalem]{ulem}
\\usepackage{booktabs}
\\usepackage{graphicx}
[NO-DEFAULT-PACKAGES]
[EXTRA]
[PACKAGES]"))

(defun org-export-koma-letter-at-subtree (dest)
  "Define a command to export the koma letter subtree at point to PDF.
With a prefix arg, prompt for the output destination. Otherwise
generate use the name of the current file to generate the
exported file's name.
The PDF will be created at DEST."
  (interactive
   (list (if current-prefix-arg
             (ido-read-file-name "Destination: " nil nil nil ".pdf")
           (concat (f-no-ext (buffer-file-name)) ".pdf"))))

  (let ((tmpfile (make-temp-file "org-export-" nil ".org")))
    (org-write-subtree-content tmpfile)
    (with-current-buffer (find-file-noselect tmpfile)
      (unwind-protect
          (-if-let (exported (org-koma-letter-export-to-pdf))
              (f-move exported dest)
            (error "Export failed"))
        (kill-buffer)))
    (%-sh "open" (%-quote dest))
    (message "opening %s..." dest)))

(add-hook 'org-ctrl-c-ctrl-c-hook
          (lambda ()
            (when (ignore-errors
                    (s-matches? (rx "latex_class:" (* space) "koma")
                                (org-subtree-content)))
              (call-interactively 'org-export-koma-letter-at-subtree)
              'export-koma-letter))
          t)

(provide 'config-org-export)

;;; config-org-export.el ends here
