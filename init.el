;;; init.el --- Bootstrap configuration

;; Copyright (C) 2013, 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.2

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

;; This file is automatically loaded by Emacs on startup in order to perform
;; user customisation. Here, we initialise the Emacs package manager and load
;; configuration files in "./lisp".

;;; Code:

(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "platform"))

(require 'utils-common)
(require 'config-base)
(require 'config-modeline)

;; Load customisations for current platform.

(pcase system-type
  (`darwin  (require 'platform-darwin))
  (`linux   (require 'platform-linux))
  (platform (warn "Unknown platform: %s" platform)))

;; Load all files in lisp dir.
;; Ignore flycheck temp files.
(dolist (f (file-expand-wildcards (concat user-emacs-directory "lisp/*.el") t))
  (let ((feature (intern (file-name-sans-extension (file-name-nondirectory f)))))
    (eval
     `(with-demoted-errors ,(concat (format "Init (%s):" feature) " %s")
        (unless (string-match-p "^flycheck_" (symbol-name feature))
          (message "--> Loading %s..." feature)
          (require feature))))))

(require 'custom)
(require 'personal-config nil t)

(unless (ignore-errors (emacs-init-time))
  (setq default-directory user-home-directory))

(provide 'init)

;;; init.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
