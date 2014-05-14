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

(when (executable-find "w3m")
  (cb:install-package 'w3m))

(after 'w3m

  (hook-fn 'w3m-mode-hook (setq line-spacing 5))

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
      (local-set-key (kbd "q") (command (restore))))))

(after '(w3m evil)
  (bind-keys
    :map w3m-mode-map
    "z t" 'evil-scroll-line-to-top
    "z b" 'evil-scroll-line-to-bottom
    "z z" 'evil-scroll-line-to-center
    "C-f" 'evil-scroll-page-down
    "C-b" 'evil-scroll-page-up
    "w"   'evil-forward-word-begin
    "b"   'evil-backward-word-begin
    "y"   'evil-yank
    "p"   'evil-paste-after
    "/"   'evil-search-forward
    "?"   'evil-search-backward
    "n"   'evil-search-next
    "N"   'evil-search-previous))

(provide 'config-w3m)

;;; config-w3m.el ends here
