;;; config-orglink.el --- Configure orglink

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

;; Configure orglink

;;; Code:

(require 'utils-common)

(cb:install-package 'orglink t)

(custom-set-variables
 '(orglink-activate-links '(angle plain))
 '(orglink-mode-lighter nil))

(hook-fns '(prog-mode-hook text-mode-hook comint-mode)
  (ignore-errors
    (unless (derived-mode-p 'org-mode 'nxml-mode 'sgml-mode 'snippet-mode)
      (orglink-mode +1))))

(provide 'config-orglink)

;;; config-orglink.el ends here
