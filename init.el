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

;; Bootstraps my Emacs configuration.
;;
;; Initialises the Emacs package manager then loads all the files in "./lisp".
;; Each file therein must `provide' itself as an elisp feature.
;;
;; This configuration will search for a feature called `personal-config'. This
;; file can be used to set sensitive variables like passwords and configuration
;; details. I keep this file in my "~/Dropbox" folder, for example.

;;; Code:

(message "Emacs %s.%s %s"
         emacs-major-version emacs-minor-version system-configuration)


(defvar cb:use-vim-keybindings? t
  "Set to nil to disable Evil-mode and associated key bindings.")


;;; Disable intrusive GUI elements.

(when (fboundp 'scroll-bar-mode)   (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)     (tool-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(when (fboundp 'menu-bar-mode)     (menu-bar-mode (if (display-graphic-p) +1 -1)))

;; Increase GC threshold to 20 MiB. Computers have lots of memory these days.
(setq gc-cons-threshold 20000000)

(require 'package)
(require 'cl)
(require 'cl-lib)

;;;; Basic paths.

(eval-and-compile
  (setq user-emacs-directory (expand-file-name user-emacs-directory))
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (add-to-list 'load-path (concat (getenv "HOME") "/Dropbox/"))
  (add-to-list 'load-path (concat user-emacs-directory "lisp")))

;; Load submodules.

(cl-loop with lib = (concat user-emacs-directory "lib/")
         for module in '("org-mode" "apel")
         do (add-to-list 'load-path (concat lib module)))

;;; Configure packages.

(cl-loop for source in
         '(("melpa"     . "http://melpa.milkbox.net/packages/")
           ("marmalade" . "http://marmalade-repo.org/packages/"))
         do (add-to-list 'package-archives source)
         finally (package-initialize))

(cl-loop for pkg in '(bind-key
                      use-package
                      diminish
                      dash dash-functional
                      s f
                      noflet
                      async
                      auto-compile
                      deferred
                      )
         initially (unless package-archive-contents (package-refresh-contents))
         unless (package-installed-p pkg)
         do (package-install pkg)
         do (require pkg))

;; Add some explicit requirements to prevent warnings.
(require 'use-package)
(require 'dash)
(require 'f)
(require 'auto-compile)
(auto-compile-on-save-mode +1)
(auto-compile-on-load-mode +1)
(setq use-package-verbose nil)

;; As a special case, remove the ELPA version of org-mode from load-path.
(-when-let (org (--first (s-matches? (rx "/org-" (+ num) eol) it)
                         (f-directories package-user-dir)))
  (cl-delete org load-path))

;; Load order-dependent core features.

(require 'cb-lib)
(require 'cb-foundation)
(require 'cb-server)
(require 'cb-mode-groups)
(require 'cb-typefaces)
(require 'cb-colour)
(require 'personal-config nil t)

;; Load remaining config files in the lisp directory.
;; Each file must declare a corresponding emacs feature.
(let* ((files (f-files cb:lisp-dir))
       (config-files (-filter (& (~ (<> f-ext?) "el")
                                 (N (| (~ s-contains? "flycheck")
                                       (~ s-ends-with? "~"))))
                              files))
       ;; Show use-package's debug messages if `use-package-verbose' is set.
       (verbose? (and (true? use-package-verbose)
                      (not after-init-time))))

  ;; Byte-compile lisp files. Skip this step if all config files have a
  ;; corresponding elc file.
  (unless (--all? (-contains? files (concat it "c")) config-files)
    (--each config-files
      (let ((inhibit-redisplay t))
        (save-window-excursion
          (byte-recompile-file it nil 0)))))

  ;; Load files.
  (run-with-progress-bar
   "Loading configuration"

   (-concat
    ;; Load config files
    (->> config-files
      (-map (C intern f-no-ext f-filename))
      (--map (eval `(lambda ()
                      (with-demoted-errors
                        (use-package ,it))))))
    ;; Load special files.
    (list
     (lambda () (load (f-join user-emacs-directory "custom.el") t t))
     (lambda () (load (concat user-emacs-directory "site-file.el") t t))))
   :silent? (not verbose?)))

;; Load special files.

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; init.el ends here
