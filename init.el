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

(auto-compile-on-save-mode +1)
(auto-compile-on-load-mode +1)
(setq use-package-verbose nil)

;; As a special case, remove elpa version of org-mode from load-path.
(-when-let (org (--first (s-matches? (rx "/org-" (+ num) eol) it)
                         (f-directories package-user-dir)))
  (cl-delete org load-path))

;; Configure el-get

(defvar el-get-sources '(wanderlust))
(defvar cb:el-get-dir (concat user-emacs-directory "el-get/"))
(add-to-list 'load-path (concat cb:el-get-dir "el-get"))
(unless (file-exists-p cb:el-get-dir)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(use-package el-get
  :defer t
  :commands (el-get
             el-get-install
             el-get-update
             el-get-list-packages)
  :init
  (defvar el-get-sources nil)

  :config
  (progn
    (defun el-get-read-status-file ()
      (mapcar #'(lambda (entry)
                  (cons (plist-get entry :symbol)
                        `(status "installed" recipe ,entry)))
              el-get-sources))

    (defalias 'el-get-init 'ignore
      "Don't use el-get for making packages available for use.")))

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
       (config-files (-filter (& (~ (^ f-ext?) "el")
                                 (N (| (~ s-contains? "flycheck")
                                       (~ s-ends-with? "~"))))
                              files))
       ;; Show use-package's debug messages if `use-package-verbose' is set.
       (verbose? (and (true? use-package-verbose)
                      (not after-init-time))))
  ;; Byte-compile lisp files. Skip this step if all config files have a
  ;; corresponding elc file.
  (unless (--all? (-contains? files (concat it "c")) config-files)
    (run-with-progress-bar
     "Byte-compiling configuration"
     (--map (eval `(lambda ()
                     (let ((inhibit-redisplay t))
                       (save-window-excursion
                         (byte-recompile-file ,it nil 0)))))
            config-files)
     :silent? (not verbose?)))
  ;; Load files.
  (run-with-progress-bar
   "Loading configuration"
   (->> config-files
     (-map (C intern f-no-ext f-filename))
     (--map (eval `(lambda () (use-package ,it)))))
   :silent? (not verbose?)))

(load (f-join user-emacs-directory "custom.el") t t)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; init.el ends here
