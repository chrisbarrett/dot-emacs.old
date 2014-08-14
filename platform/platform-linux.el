;;; platform-linux.el --- Configuration for linux hosts.

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

;; Configuration for linux hosts.

;;; Code:

(when (equal system-type 'gnu/linux)
  (cb:install-package 'exec-path-from-shell t)
  (exec-path-from-shell-initialize)

  (bind-key* "C-S-v" 'yank))

(provide 'platform-linux)

;;; platform-linux.el ends here
