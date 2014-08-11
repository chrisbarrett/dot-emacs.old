;;; config-w3m.el --- Configure w3m

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

;; Configure w3m

;;; Code:

(require 'utils-common)
(require 'utils-ui)
(autoload 'thing-at-point-url-at-point "thingatpt")

(when (executable-find "w3m") (cb:install-package 'w3m))

(put 'w3m-mode 'line-spacing 5)

(declare-modal-executor w3m
  :command w3m
  :bind "M-W"
  :restore-bindings '("M-W" "M-E"))

(defun w3m-browse-dwim (url)
  "Browse to URL, ensuring it begins with http:// as required by w3m."
  (interactive
   (list
    (read-string "Go to URL: "
                 (thing-at-point-url-at-point)
                 t)))
  (with-window-restore
    (w3m-browse-url
     (if (s-starts-with? "http://" url)
         url
       (concat "http://" url)))
    (local-set-key (kbd "q") (command (restore)))))

(provide 'config-w3m)

;;; config-w3m.el ends here
