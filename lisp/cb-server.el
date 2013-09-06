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
        (let ((fm (or (car frame) (selected-frame)))
              (tranparent "ARGBBB000000")
              (blue "#168DCC")
              )
          (set-face-foreground 'default "black" fm)
          (set-face-background 'default tranparent fm)
          (set-face-background 'menu blue fm)
          (set-face-foreground 'menu "white" fm)
          (set-face-background 'hl-line tranparent fm)
          (set-face-background 'fringe tranparent fm)
          (set-face-background 'cursor "#2F4F4F" fm)
          ;; Modeline
          (set-face-foreground 'mode-line-filename "white" fm)
          (set-face-foreground 'mode-line-position "white" fm)
          (set-face-background 'mode-line blue fm)
          (set-face-background 'mode-line blue fm)
          ;; Org
          (when (boundp 'org-block-background)
            (set-face-background 'org-block-background tranparent fm)))))

    (defadvice server-create-window-system-frame (after configure-frame activate)
      "Set custom frame colours when creating the first frame on a display"
      (cb-server:configure-frame))

    (add-hook 'after-make-frame-functions 'cb-server:configure-frame t)))


(provide 'cb-server)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-server.el ends here
