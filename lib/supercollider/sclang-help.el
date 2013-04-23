;;; sclang-help --- Help file handling for SuperCollider.

;; Copyright (C) 2013 Chris Barrett

;; Author:

;; This file is not part of GNU Emacs.

;; copyright 2003 stefan kersten <steve@k-hornz.de>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
;; USA

;;; Commentary:

;; Help file handling for SuperCollider.
;; Updated by Chris Barrett <chris.d.barrett@me.com>

;;; Code:

(require 'sclang-util)
(require 'sclang-interp)
(require 'sclang-language)
(require 'sclang-mode)
(require 'sclang-vars)
(require 'sclang-minor-mode)
(require 'dash)
(require 's)
(require 'cl-lib)

(autoload 'w3m-edit-current-url "w3m")

;;; ----------------------------------------------------------------------------
;;; Customizable variables.

(defcustom sclang-help-path
  (list sclang-system-help-dir "~/.local/share/SuperCollider/Help")
  "*List of directories where SuperCollider help files are kept."
  :group 'sclang-interface
  :version "21.4"
  :type '(repeat directory))

(defconst sclang-extension-path
  (list sclang-system-extension-dir "~/.local/share/SuperCollider/Extensions")
  "List of SuperCollider extension directories.")

;;; ----------------------------------------------------------------------------
;;; Help file caching

(defvar sclang-scdoc-topics (make-hash-table :size 16385)
  "List of all scdoc topics.")

(defvar sclang-help-topic-alist nil
  "Alist mapping help topics to file names.")

(defvar sclang-help-topic-history nil
  "List of recently invoked help topics.")

(defconst sclang-special-help-topics
  '(("/" . "division")
    ("-" . "subtraction"))
  "Alist of help topics with transcoded filenames.")

(defun sclang-help-topic-name (file)
  "Use the name of FILE to determine the title of the corresponding help topic."
  (->> file
    (file-name-nondirectory)
    (file-name-sans-extension)))

;; =====================================================================
;; help file access
;; =====================================================================

(defun sclang-skip-help-directory? (path)
  "Answer t if PATH should be skipped during help file indexing."
  (let ((directory (file-name-nondirectory path)))
    (-reduce (lambda (a b) (or a b))
            (mapcar (lambda (regexp) (string-match regexp directory))
                    '("^\.$" "^\.\.$" "^CVS$" "^\.svn$" "^_darcs$")))))

(defun sclang-filter-help-directories (list)
  "Remove paths to be skipped from LIST of directories."
  (--remove (or (not (file-directory-p it))
                (sclang-skip-help-directory? it))
            list))

(defun sclang-help-directories ()
  "Answer list of help directories to be indexed."
  (append sclang-help-path sclang-extension-path))

(defun sclang-help-files (dirs)
  "Find the list of help files in DIRS."
  (->> dirs
    (--mapcat (directory-files it t))
    (--filter (equal "schelp" (file-name-extension it)))))

(defun sclang-make-help-topic-alist (dirs result)
  "Build a help topic alist from directories in DIRS, with initial RESULT."
  (if dirs
      (let* ((files (sclang-help-files dirs))
             (topics (-remove 'null (mapcar 'sclang-help-topic-name files)))
             (new-dirs	(sclang-filter-help-directories files)))
        (sclang-make-help-topic-alist
         (append new-dirs (cdr dirs))
         (append topics result)))
    (sort result (lambda (a b) (string< (car a) (car b))))))

(defun sclang-index-help-topics ()
  "Build an index of help topics searching in the various help file locations."
  (interactive)
  (setq sclang-help-topic-alist nil)
  (let ((case-fold-search nil)
        (max-specpdl-size 10000)
        (max-lisp-eval-depth 10000))
    (sclang-message "Indexing help topics ...")
    (setq sclang-help-topic-alist
          (sclang-make-help-topic-alist (sclang-help-directories) nil))
    (sclang-message "Indexing help topics ... Done")))

(defun sclang--read-help-topic ()
  "Read an SCDoc topic, with a default value."
  (let* ((topic (sclang-symbol-at-point))
         (default (if topic (format " (default %s)" topic) ""))
         (prompt (format "Help topic%s: " default)))
    (completing-read prompt
                     sclang-scdoc-topics nil nil nil
                     'sclang-help-topic-history topic)))

(cl-defun sclang--blocking-eval-string (expr &optional (timeout-ms 100))
  "Ask SuperCollider to evalutate the given string EXPR. Wait a maximum TIMEOUT-MS."
  (let ((result nil)
        (elapsed 0))
    ;; SuperCollider will eval the string and then call back with the result.
    ;; We rebind Emacs' `message' action to intercept the response.
    (flet ((message (str &rest _) (setq result str)))
      (sclang-eval-string (format "Emacs.message(%s)" expr))
      ;; Block until we receive a response or the timeout expires.
      (while (and (not result) (> timeout-ms elapsed))
        (sleep-for 0 10)
        (setq elapsed (+ 10 elapsed)))
      result)))

(defun sclang--topic->helpfile (topic)
  "Get the absolute path to the SuperCollider help file for TOPIC."
  (->> (format "SCDoc.findHelpFile(\"%s\")" topic)
    (sclang--blocking-eval-string)
    (s-chop-prefix "file://")))

(defun sclang-find-help (topic)
  "Prompt the user for a help TOPIC."
  (interactive (list (sclang--read-help-topic)))
  (let ((file (sclang--topic->helpfile topic)))
    (if file
        (w3m-browse-url file)
      (error "No help for \"%s\"" topic)) nil))

(defun sclang-open-help-gui ()
  "Open SCDoc Help Browser."
  (interactive)
  (sclang-eval-string (sclang-format "Help.gui")))

(defun sclang-find-help-in-gui (topic)
  "Search for TOPIC in SCDoc Help Browser."
  (interactive (list (sclang--read-help-topic)))
  (if topic
      (sclang-eval-string (sclang-format "HelpBrowser.openHelpFor(%o)" topic))
    (sclang-eval-string (sclang-format "Help.gui"))))

(sclang-set-command-handler
 'helpSymbols
 (lambda (syms)
   (--map (puthash it nil sclang-scdoc-topics) syms)))

;;; -----------------------------------------------------------------------------
;;; Setup modle.
;;;
;;; Ensure that help system is set up and torn down with the SuperCollider server.

(defun sclang--setup-help ()
  "Initialize help system."
  (sclang-perform-command 'helpSymbols)
  (ignore-errors (sclang-index-help-topics)))

(defun sclang--clean-up-help ()
  "Tear down help system."
  (clrhash sclang-scdoc-topics))

(add-hook 'sclang-library-startup-hook 'sclang--setup-help)
(add-hook 'sclang-library-shutdown-hook 'sclang--clean-up-help)

(provide 'sclang-help)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; sclang-help.el ends here
