;;; cb-elisp --- Commands for Elisp editing

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

;; Commands for Elisp editing

;;; Code:

;;; Font lock

(autoload 'ielm "ielm")
(autoload 'projectile-project-root "projectile")
(require 'dash)

(defun cb:switch-to-ielm ()
  "Start up or switch to an Inferior Emacs Lisp buffer."
  (interactive)
  ;; HACK: rebind switch-to-buffer so ielm opens in another window.
  (flet ((switch-to-buffer (buf) (switch-to-buffer-other-window buf)))
    (ielm)
    (goto-char (point-max))
    (when (fboundp 'evil-append-line)
      (evil-append-line 1))))

(defun cb:last-elisp-buffer ()
  "Find the last active Elisp buffer."
  (--first (with-current-buffer it
             (equal 'emacs-lisp-mode major-mode))
           (buffer-list)))

(defun cb:switch-to-elisp ()
  "Switch to the last active elisp buffer."
  (interactive)
  (-when-let (buf (cb:last-elisp-buffer))
    (switch-to-buffer-other-window buf)))

(defun cb:find-and-load-ert-tests ()
  "Load the test-runner for the current project if one exists."
  (interactive)
  (-when-let (root (projectile-project-root))
    (let* (
           (files (->> (list root (concat root "test/") (concat root "tests/"))
                    ;; Find all tests in possible test directories.
                    (-filter 'file-exists-p)
                    (--mapcat (directory-files it t (rx "test" (* nonl) ".el")))
                    (-remove 'null)
                    (-filter 'file-exists-p)
                    ;; Find test runners.
                    (--group-by (s-matches? (rx (or "runner" "fixture")) it))))
           (runners (assoc t files))
           (tests   (assoc nil files))
           )
      (-each (cdr runners) 'load-file)
      (-each (cdr tests)   'load-file)
      (when (or runners tests)
        (message "Loaded %s test files"
                 (+ (length (cdr runners))
                    (length (cdr tests))))))))

(provide 'cb-elisp)

;;; cb-elisp.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; End:
