;;; config-org-mac-link.el --- Configuration for org-mac-link.

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

;; Configuration for org-mac-link.

;;; Code:

(when (equal system-type 'darwin)
  (require 'org-mac-link)
  (require 'org)
  (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link))

(provide 'config-org-mac-link)

;;; config-org-mac-link.el ends here
