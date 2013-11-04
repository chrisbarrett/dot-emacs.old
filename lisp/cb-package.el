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

(defvar cbpkg:package-icon (f-join cb:assets-dir "package.png"))

(defun cbpkg:install-packages (pkgs)
  ;; Show summary of packages to be installed.
  (-when-let (len (and pkgs (length pkgs)))
    (growl "Installing Packages"
           (format "%s package%s will be installed or updated:\n%s"
                   len
                   (if (= 1 len) "" "s")
                   (s-join ", " (-map 'pp-to-string pkgs)))
           cbpkg:package-icon)
    ;; Perform installation.
    (-each pkgs 'package-install)))

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

;; Install packages asynchronously from the package menu.

(defun cbpkg:y-or-n? (verb pkgs)
  "Prompt the user for confirmation about a package action.
* VERB is the action to perform.
* PKGS is a list of package names."
  (yes-or-no-p
   (if (= (length pkgs) 1)
       (format "%s package `%s'? " verb (car pkgs))
     (format "%s these %d packages (%s)? "
             verb (length pkgs) (s-join ", " (-map 'prin1-to-string pkgs))))))

(defun cbpkg:delete-marked-packages (pkgs)
  "Prompt user and delete PKGS."
  (when (and pkgs (cbpkg:y-or-n? "Delete" pkgs))
    (dolist (elt pkgs)
      (condition-case-unless-debug err
          (package-delete (car elt) (cdr elt))
        (error (message (cadr err)))))))

(defun cbpkg:install-packages-async (pkgs)
  "Prompt user and install PKGS in the background."
  (when (and pkgs (cbpkg:y-or-n? "Install" pkgs))
    ;; Lexically bind PKGS so the list can be injected.
    (let ((pkgs pkgs))
      (async-start

       `(lambda ()
          ,(async-inject-variables "^pkgs$")
          ,(async-inject-variables "^package-")
          ,(async-inject-variables "^load-path$")
          (require 'cb-package)
          (package-initialize)
          (cbpkg:install-packages pkgs)
          (length pkgs))

       (lambda (len)
         (package-initialize)
         (growl "Installation Finished"
                (concat
                 (format "%s package%s were installed " len (if (= 1 len) "" "s"))
                 "and will be loaded next time Emacs is started.")
                cbpkg:package-icon))))))

(defun cbpkg:packages-in-menu ()
  "Return the packages in the package menu associated with an action.
It is a list of the from (ACTION PACKAGE-NAME VERSIONS) The
symbol ACTION is one of skip, delete or install, depending on
whether the user has marked the package."
  (save-excursion
    (cl-loop
     initially (goto-char (point-min))
     while (not (eobp))
     for (name . versions) = (tabulated-list-get-id)
     for action = (cl-case (char-after)
                    (?\s 'skip)
                    (?D 'delete)
                    (?I 'install))
     collect (list action name versions)
     do (forward-line))))

(defun package-menu-execute-async ()
  "Perform marked Package Menu actions.
Packages marked for installation are downloaded and installed;
packages marked for deletion are removed.
Any packages marked for installation or updating will be processed in the background."
  (interactive)
  (unless (derived-mode-p 'package-menu-mode)
    (error "The current buffer is not in Package Menu mode"))

  (let ((pkgs (cbpkg:packages-in-menu)))
    ;; Perform actions on marked packages.
    (cbpkg:install-packages-async
     (->> pkgs
       (-filter (C (~ equal 'install) car))
       (-map (lambda+ ((_ name _)) name))))
    (cbpkg:delete-marked-packages
     (->> pkgs
       (-filter (C (~ equal 'delete) car))
       (-map (lambda+ ((_ name vs))
               (cons (symbol-name name) (package-version-join vs))))))
    (package-initialize)
    (package-menu--generate t t)))

(after 'package
  (define-key package-menu-mode-map (kbd "x") 'package-menu-execute-async))

(provide 'cb-package)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-package.el ends here
