;;; cb-overtone.el --- Minor mode for Overtone.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130510.1410

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

;; Minor mode for Overtone.

;;; Code:

(autoload 'nrepl-eval "nrepl")
(autoload 'nrepl-mode-map "nrepl")

(defvar overtone-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-g") 'cb:stop-overtone)
    (define-key km (kbd "s-.") 'cb:stop-overtone)
    km))

(define-minor-mode overtone-mode
  "Provide additional overtone-related functionality for clojure."
  nil " overtone" overtone-mode-map
  (require 'nrepl)
  (when (boundp 'nrepl-mode-map)
    (define-key nrepl-mode-map (kbd "C-c C-g") 'cb:stop-overtone)
    (define-key nrepl-mode-map (kbd "S-.") 'cb:stop-overtone))
  ;; Jack in if there's no active connection.
  (unless (and (boundp 'nrepl-connection-list) nrepl-connection-list)
    (nrepl-jack-in)))

(defun maybe-enable-overtone-mode ()
  "Enable `overtone-mode' only if the current Clojure buffer references overtone."
  (when (and (not overtone-mode)
             (derived-mode-p 'clojure-mode)
             (string-match-p "overtone.live" (buffer-string)))
    (overtone-mode t)))

(defun cb:stop-overtone ()
  "Stop synthesis."
  (interactive)
  (nrepl-eval "(stop)")
  (message "Synthesis stopped."))

(add-hook 'clojure-mode-hook 'maybe-enable-overtone-mode)
(add-hook 'after-save-hook 'maybe-enable-overtone-mode)

(provide 'cb-overtone)

;; Local Variables:
;; End:

;;; cb-overtone.el ends here
