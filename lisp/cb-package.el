;;; cb-package.el --- Extensions to package.el

;; Copyright (C) 2013 Chris Barrett

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

;; Extensions to package.el

;;; Code:

(require 'cl-lib)
(require 'cb-paths)
(require 'package)
(require 'cb-osx)

(defvar cbpkg:package-icon (f-join cb:assets-dir "cask.png"))

(hook-fn 'package-background-installation-finished-hook
  :arglist (pkg)
  (growl "Package Installed"
         (format "%s installed successfully." pkg)
         cbpkg:package-icon))

(hook-fn 'package-background-installation-started-hook
  :arglist (pkgs)
  (-when-let (len (and pkgs (length pkgs)))
    (growl "Installing Packages"
           (format "%s package%s will be installed or updated." len (if (= 1 len) "" "s"))
           cbpkg:package-icon)))

(defun cbpkg:install-packages (pkgs)
  (run-hook-with-args 'package-background-installation-started-hook pkgs)
  (--each pkgs
    (package-install it)
    (run-hook-with-args 'package-background-installation-finished-hook it)))

(defun update-packages ()
  "Update all installed packages in the background."
  (interactive)
  (growl "Starting Updates"
         "Updating packages in the background."
         cbpkg:package-icon)
  (async-start

   `(lambda ()
      ,(async-inject-variables "^package-")
      ,(async-inject-variables "^load-path$")
      (require 'cb-package)
      (package-refresh-contents)
      (package-initialize)
      (let ((pkgs (cbpkg:updateable-packages)))
        (cbpkg:install-packages pkgs)
        (length pkgs)))

   (lambda (len)
     (package-initialize)
     (if (zerop len)
         (growl "Updates Finished"
                "No packages needed to be updated."
                cbpkg:package-icon)

       (growl "Updates Finished"
              (concat
               (format "%s package%s were updated " len (if (= 1 len) "" "s"))
               "and will be loaded next time Emacs is started.")
              cbpkg:package-icon)))))

(defun cbpkg:updateable-packages ()
  "Return the packages with available updates."
  ;; The easiest way to get this info is from the package menu.
  (-keep 'car
         (save-window-excursion
           (save-excursion
            (package-list-packages t)
            (package-menu--find-upgrades)))))

(provide 'cb-package)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-package.el ends here
