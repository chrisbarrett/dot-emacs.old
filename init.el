;;; init.el --- My emacs configuration

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; My emacs configuration.

;;; Code:

(message "Emacs %s.%s %s"
         emacs-major-version emacs-minor-version system-configuration)


(defvar cb:use-vim-keybindings? t
  "Set to nil to disable Evil-mode and associated key bindings.")


;;; Disable intrusive GUI elements.

(scroll-bar-mode   -1)
(tool-bar-mode     -1)
(blink-cursor-mode -1)
(menu-bar-mode (if (display-graphic-p) +1 -1))

;; Increase GC threshold. Computers have lots of memory these days.
(setq gc-cons-threshold 20000000)

(require 'package)
(require 'cl)
(require 'cl-lib)

;;;; Basic paths.

(setq user-emacs-directory (expand-file-name user-emacs-directory))
(add-to-list 'load-path (concat (getenv "HOME") "/Dropbox/"))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(delete-if (lambda (f) (string-match-p (rx "/lisp/org" eol) f)) load-path)
(add-to-list 'load-path (concat user-emacs-directory "etc/org-mode/lisp"))

;;; Configure packages.

(cl-loop for source in
         '(("melpa"     . "http://melpa.milkbox.net/packages/")
           ("marmalade" . "http://marmalade-repo.org/packages/"))
         do (add-to-list 'package-archives source)
         finally (package-initialize))

(cl-loop for pkg in '(bind-key use-package diminish s dash cl-lib f noflet)
         initially (unless package-archive-contents (package-refresh-contents))
         unless (package-installed-p pkg)
         do (package-install pkg))

(require 'use-package)
(setq use-package-verbose nil)

(use-package cb-lib)
(use-package cb-foundation)
(use-package personal-config)
(use-package cb-mode-groups)
(use-package cb-evil :if cb:use-vim-keybindings?)
(use-package cb-typefaces)
(use-package cb-modeline)
(use-package cb-osx :if (equal system-type 'darwin))
(use-package cb-helm)
(use-package cb-ido)
(use-package cb-commands)
(use-package cb-window-management)
(use-package cb-backups)
(use-package cb-cosmetic)
(use-package cb-colour :if (or (daemonp) (display-graphic-p)))
(use-package cb-smartparens)
(use-package cb-web)
(use-package cb-gnus)
(use-package cb-shell)
(use-package cb-completion)
(use-package cb-dired)
(use-package cb-compilation)
(use-package cb-ctags)
(use-package cb-language-utils)
(use-package cb-markup)
(use-package cb-lisp)
(use-package cb-elisp)
(use-package cb-clojure)
(use-package cb-overtone)
(use-package cb-scheme)
(use-package cb-python)
(use-package cb-ruby)
(use-package cb-haskell)
(use-package cb-clang)
(use-package cb-supercollider)
(use-package cb-asm)
(use-package cb-misc-languages)
(use-package cb-git)
(use-package cb-org)
(use-package cb-productivity)
(use-package cb-fortune)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; init.el ends here
