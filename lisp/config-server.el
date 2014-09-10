;;; config-server.el --- Configuration for emacs server

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

;; Configuration for emacs server

;;; Code:

(require 'server)
(require 'utils-common)

(hook-fn 'after-init-hook
  (unless (or noninteractive (server-running-p))
    (server-start)))

(defun cb-server:set-color-theme (frame)
  (select-frame frame)
  (unless (window-system frame)
    (-each custom-enabled-themes 'disable-theme)))

(add-hook 'after-make-frame-functions 'cb-server:set-color-theme)

(provide 'config-server)

;;; config-server.el ends here
