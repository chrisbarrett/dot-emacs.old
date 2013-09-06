;;; cb-server.el --- Configuration for emacs server.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130906.0910

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

;; Configuration for emacs server.

;;; Code:

(autoload 'server-running-p "server")

(hook-fn 'after-init-hook
  (unless (server-running-p)
    (server-start)))

(use-package server
  :defer t
  :config
  (progn

    (defun cb-server:configure-frame (&rest frame)
      "Disable themeing for console emacsclient."
      (unless (display-graphic-p)
        (let ((fm (or (car frame) (selected-frame))))
          (set-face-foreground 'default "#FFFFFF" fm)
          (set-face-background 'fringe  "#000000" fm)
          (set-face-background 'cursor "#2F4F4F" fm)
          (set-face-background 'mode-line "#2F4F4F" fm)
          (set-face-foreground 'mode-line "#BCBf91" fm))))

    (defadvice server-create-window-system-frame (after configure-frame activate)
      "Set custom frame colours when creating the first frame on a display"
      (cb-server:configure-frame))

    (add-hook 'after-make-frame-functions 'cb-server:configure-frame t)))


(provide 'cb-server)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-server.el ends here
