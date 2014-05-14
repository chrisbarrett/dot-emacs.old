;;; config-org-crypt.el --- Configure org-crypt

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

;; Configure org-crypt

;;; Code:

(require 'org-crypt)

(custom-set-variables
 '(org-crypt-disable-auto-save 'encypt))

(org-crypt-use-before-save-magic)

(add-to-list 'org-tags-exclude-from-inheritance "crypt")

(defun cb-org:looking-at-pgp-section? ()
  (unless (org-before-first-heading-p)
    (save-excursion
      (org-back-to-heading t)
      (let ((heading-point (point))
            (heading-was-invisible-p
             (save-excursion
               (outline-end-of-heading)
               (outline-invisible-p))))
        (forward-line)
        (looking-at "-----BEGIN PGP MESSAGE-----")))))

(defun cb-org:decrypt-entry ()
  (when (cb-org:looking-at-pgp-section?)
    (org-decrypt-entry)
    t))

(add-hook 'org-ctrl-c-ctrl-c-hook 'cb-org:decrypt-entry)

(provide 'config-org-crypt)

;;; config-org-crypt.el ends here
