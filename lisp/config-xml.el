;;; config-xml.el --- Configure XML

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

;; Configure XML

;;; Code:

(require 'utils-common)

(hook-fn 'find-file-hook
  (when (s-starts-with? "<?xml " (buffer-string))
    (nxml-mode)))

(defun tidy-xml-buffer ()
  "Reformat the current XML buffer using Tidy."
  (interactive)
  (save-excursion
    (call-process-region (point-min) (point-max) "tidy" t t nil
                         "-xml" "-i" "-wrap" "0" "-omit" "-q")))

(after 'nxml-mode
  (define-key nxml-mode-map (kbd "M-q") 'tidy-xml-buffer))

(setq-default sgml-xml-mode t)

(after 'sgml-mode
  (define-key sgml-mode-map (kbd "M-q") 'tidy-xml-buffer))

(provide 'config-xml)

;;; config-xml.el ends here
