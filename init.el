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
(setq custom-file (concat user-emacs-directory "custom.el"))
(add-to-list 'load-path (concat (getenv "HOME") "/Dropbox/"))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; As a special case, ensure we're using the latest version of org-mode and not
;; the version shipped with Emacs.
(let ((org-src  (concat user-emacs-directory "etc/org-mode/lisp"))
      (default-directory user-emacs-directory))
  (unless (file-exists-p org-src)
    (async-shell-command "make org" "*make org*"))
  (add-to-list 'load-path org-src))

;;; Configure packages.

(loop for source in
      '(("melpa"     . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/"))
      do (add-to-list 'package-archives source)
      finally (package-initialize))

(loop for pkg in '(bind-key use-package diminish dash s f noflet async)
      initially (unless package-archive-contents (package-refresh-contents))
      unless (package-installed-p pkg)
      do (package-install pkg))

(require 'use-package)
(setq use-package-verbose nil)

(require 'cb-lib)
(require 'cb-foundation)
(require 'personal-config)
(require 'cb-mode-groups)
(if cb:use-vim-keybindings?  (require 'cb-evil))
(require 'cb-typefaces)
(require 'cb-modeline)
(if (equal system-type 'darwin) (require 'cb-osx))
(require 'cb-helm)
(require 'cb-ido)
(require 'cb-commands)
(require 'cb-window-management)
(require 'cb-backups)
(require 'cb-cosmetic)
(if (or (daemonp) (display-graphic-p)) (require 'cb-colour))
(require 'cb-smartparens)
(require 'cb-net)
(require 'cb-gnus)
(require 'cb-shell)
(require 'cb-yasnippet)
(require 'cb-autocomplete)
(require 'cb-dired)
(require 'cb-compilation)
(require 'cb-ctags)
(require 'cb-language-utils)
(require 'cb-markup)
(require 'cb-lisp)
(require 'cb-elisp)
(require 'cb-clojure)
(require 'cb-overtone)
(require 'cb-scheme)
(require 'cb-python)
(require 'cb-ruby)
(require 'cb-haskell)
(require 'cb-clang)
(require 'cb-supercollider)
(require 'cb-asm)
(require 'cb-misc-languages)
(require 'cb-git)
(require 'cb-org)
(require 'cb-productivity)
(require 'cb-fortune)
(require 'custom)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; init.el ends here
