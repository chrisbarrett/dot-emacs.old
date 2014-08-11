;;; config-info.el --- Configuration for info

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

;; Configuration for info

;;; Code:

(require 'utils-common)

(custom-set-variables
 '(Info-additional-directory-list
   (-flatten (list cb:info-dir
                   (f-join source-directory "info/")
                   (f-directories cb:info-dir)))))

;; NB: line spacing is measured in pixels.
(put 'Info-mode 'line-spacing 2)

;;; Key bindings

(after 'info
  (define-key Info-mode-map (kbd "SPC") 'Info-scroll-up)
  (define-key Info-mode-map (kbd "S-SPC") 'Info-scroll-down))

(provide 'config-info)

;;; config-info.el ends here
