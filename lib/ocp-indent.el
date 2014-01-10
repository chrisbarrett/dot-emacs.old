;;; ocp-indent.el --- Automatically indents tuareg buffers using ocp-indent

;; Copyright (C) 2014 OCAMLPro

;; Author: OCAMLPro <contact@ocamlpro.com>
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

;; Automatically indents tuareg buffers using ocp-indent

;;; Code:

(provide 'ocp-indent)
(require 'cl)

(defgroup ocp-indent nil
  "ocp-indent OCaml indenter binding configuration"
  :group 'languages)

(defcustom ocp-indent-path "ocp-indent"
  "*Path to access the ocp-indent command."
  :group 'ocp-indent :type '(file))

(defcustom ocp-indent-config nil
  "*Ocp-indent config string, as for its --config option.
WARNING: DEPRECATED, this will override any user or project
ocp-indent configuration files"
  :group 'ocp-indent
  :type '(choice (const nil) (string)))

(defcustom ocp-indent-syntax nil
  "*Enabled syntax extensions for ocp-indent (see option --syntax)."
  :group 'ocp-indent
  :type '(repeat (string)))

(defcustom ocp-indent-allow-tabs nil
  "*Allow `indent-tabs-mode' in OCaml buffers.
Not recommended, won't work well."
  :group 'ocp-indent
  :type '(bool))

(defun ocp--in-indentation-p ()
  "Test whether all characters between beginning of line and point are blanks."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defun ocp--indent-args (start-line end-line)
  "Format arguments to ocp-indent.
START-LINE and END-LINE define the region to be indented."
  (append
   (list "--numeric"
         "--lines" (format "%d-%d" start-line end-line))
   (if ocp-indent-config (list "--config" ocp-indent-config) nil)
   (reduce (lambda (acc syn) (list* "--syntax" syn acc))
           ocp-indent-syntax :initial-value nil)))

(defvar ocp--error-file nil
  "File used for indentation errors.")

(defun ocp--indent-file-to-string (file)
  "Read FILE, indent, and return as a string."
  (replace-regexp-in-string
   "\n$" ""
   (with-temp-buffer (insert-file-contents ocp--error-file)
                     (buffer-string))))

(defun ocp-indent-region (start end)
  "Indent the current region from START to END."
  (interactive "r")
  (setq ocp--error-file (make-temp-name "ocp-indent-error"))
  (let* ((start-line (line-number-at-pos start))
         (end-line (line-number-at-pos end))
         (indents-str
          (with-output-to-string
            (if (/= 0
                    (apply 'call-process-region
                           (point-min) (point-max) ocp-indent-path nil
                           (list standard-output ocp--error-file) nil
                           (ocp--indent-args start-line end-line)))
                (error "Can't indent: command returned failure"))))
         (indents (mapcar 'string-to-number (split-string indents-str))))
    (when (file-exists-p ocp--error-file)
      (message (ocp--indent-file-to-string ocp--error-file))
      (delete-file ocp--error-file))
    (save-excursion
      (goto-char start)
      (mapc
       #'(lambda (indent) (indent-line-to indent) (forward-line))
       indents))
    (when (ocp--in-indentation-p) (back-to-indentation))))

(defun ocp-indent-line ()
  "Indent the current line using ocp-indent."
  (interactive)
  (ocp-indent-region (point) (point)))

(defun ocp-setup-indent ()
  "Configure indentation commands to use ocp-indent."
  (interactive nil)
  (unless ocp-indent-allow-tabs (set 'indent-tabs-mode nil))
  (set (make-local-variable 'indent-line-function) #'ocp-indent-line)
  (set (make-local-variable 'indent-region-function) #'ocp-indent-region))

(add-hook 'tuareg-mode-hook 'ocp-setup-indent t)
(add-hook 'caml-mode-hook
          '(lambda ()
             (ocp-setup-indent)
             (local-unset-key "\t")) ;; caml-mode rebinds TAB !
          t)

(provide 'ocp-indent)

;;; ocp-indent.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
