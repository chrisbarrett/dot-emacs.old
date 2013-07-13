;;; cb-osx.el --- Configuration for OS X

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0024

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

;; Configuration for OS X

;;; Code:

(require 'use-package)
(require 'cb-lib)

(use-package exec-path-from-shell
  :ensure t
  :defer  t
  :if     (or (daemonp) (window-system))
  :init   (hook-fn 'after-init-hook (require 'exec-path-from-shell))
  :config
  (progn
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PKG_CONFIG_PATH")
    (exec-path-from-shell-copy-env "CFLAGS")))

;; Enable mouse support in terminal.
(use-package mouse
  :if (not (display-graphic-p))
  :defines (mouse-sel-mode)
  :config
  (progn
    (xterm-mouse-mode t)
    (defun track-mouse (_))
    (global-set-key [mouse-4] (command (scroll-down 1)))
    (global-set-key [mouse-5] (command (scroll-up 1)))))

;; Set terminfo so ansi-term displays shells correctly.

(let ((terminfo (expand-file-name "~/.terminfo")))
  (unless (file-exists-p terminfo)
    (start-process
     "tic" " tic" "tic"
     "-o" terminfo
     "/Applications/Emacs.app/Contents/Resources/etc/e/eterm-color.ti")))

;; Use system clipboard.

(unless window-system

  (defun cb:osx-paste ()
    (shell-command-to-string "pbpaste"))

  (defun cb:osx-copy (text &optional _push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function   'cb:osx-copy
        interprogram-paste-function 'cb:osx-paste))

(use-package org-mac-iCal
  :ensure t
  :commands (org-mac-iCal)
  :init
  (after 'org-agenda
    (add-to-list 'org-agenda-custom-commands
                 '("I" "Import diary from iCal" agenda ""
                   ((org-agenda-mode-hook
                     (lambda ()
                       (org-mac-iCal))))))))

(provide 'cb-osx)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-osx.el ends here
