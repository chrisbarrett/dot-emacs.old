;;; cb-cosmetic.el --- Configure, my pretties!

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0028

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

;; Configure, my pretties!

;;; Code:

(require 'use-package)

(use-package highlight
  :ensure t
  :defer t)

(use-package hl-line
  :if (or (daemonp) (display-graphic-p))
  :config (global-hl-line-mode t))

(use-package fringe
  :idle     (require 'fringe)
  :commands fringe-mode
  :config   (fringe-mode '(2 . 0)))

(use-package ansi-color
  :defer t
  :init
  (progn
    (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
    (add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on))
  :config
  (defadvice display-message-or-buffer (before ansi-color activate)
    "Process ANSI color codes in shell output."
    (let ((buf (ad-get-arg 0)))
      (and (bufferp buf)
           (string= (buffer-name buf) "*Shell Command Output*")
           (with-current-buffer buf
             (ansi-color-apply-on-region (point-min) (point-max)))))))


(provide 'cb-cosmetic)

;; Local Variables:
;; End:

;;; cb-cosmetic.el ends here
