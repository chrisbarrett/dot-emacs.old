;;; sclang-help --- Help file handling for SuperCollider.

;; Copyright (C) 2013 Chris Barrett

;; Author: stefan kersten <steve@k-hornz.de>

;; This file is not part of GNU Emacs.

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

(require 'dash)
(require 's)
(require 'cl-lib)

(autoload 'w3m-browse-url "w3m")
(autoload 'sclang-format "sclang-language")
(autoload 'sclang-eval-string "sclang-interp")
(autoload 'sclang-symbol-at-point "sclang-language")
(autoload 'sclang-set-command-handler "sclang-interp")
(autoload 'sclang-perform-command "sclang-interp")

;;; ----------------------------------------------------------------------------
;;; Customizable variables.

(defgroup sclang-help nil
  "Emacs interface to the SuperCollider help system."
  :prefix "sclang-"
  :version "21.4"
  :group 'languages)

(defcustom sclang-help-path
  (list "~/.local/share/SuperCollider/Help")
  "*List of directories where SuperCollider help files are kept."
  :group 'sclang-help
  :type '(repeat directory))

(defcustom sclang-extension-path
  (list "~/.local/share/SuperCollider/Extensions")
  "List of SuperCollider extension directories."
  :group 'sclang-help
  :type '(repeat directory))

(defcustom sclang-minibuffer-doc-idle-delay 0.25
  "The time in milliseconds to wait for idle before showing
documentation in the minibuffer for the thing at point."
  :group 'sclang-help)

;;; ----------------------------------------------------------------------------

(defvar sclang-help-topic-alist nil
  "Alist mapping help topics to file names.")

(defvar sclang-help-topic-history nil
  "List of recently invoked help topics.")

(defvar sclang-scdoc-topics (make-hash-table :size 16385)
  "List of all scdoc topics.")

(defconst sclang-special-help-topics
  '(("/" . "division")
    ("-" . "subtraction"))
  "Alist of help topics with transcoded filenames.")

(defconst sclang--help-buffer-name "*SuperCollider Help*")

;;; -----------------------------------------------------------------------------
;;; Initialization
;;;
;;; Ensure that help system is set up and torn down with the SuperCollider server.

(defun sclang--setup-help ()
  "Initialize help system."
  (sclang-perform-command 'helpSymbols)
  (sclang-eval-string "SCDoc.indexAllDocuments")
  (ignore-errors (sclang-index-help-topics)))

(defun sclang--clean-up-help ()
  "Tear down help system."
  (clrhash sclang-scdoc-topics))

(add-hook 'sclang-library-startup-hook 'sclang--setup-help)
(add-hook 'sclang-library-shutdown-hook 'sclang--clean-up-help)

(sclang-set-command-handler
 'helpSymbols (lambda (syms) (--map (puthash it nil sclang-scdoc-topics) syms)))

;;; ----------------------------------------------------------------------------
;;; Reflection

(defun sclang-typeof-expr (expr)
  "Return the class of the given SuperCollider expression EXPR."
  (->> (s-trim expr) (format "%s.class") (sclang--blocking-eval-string)))

;;; ----------------------------------------------------------------------------
;;; Help commands

(defun sclang--filename->topic (file)
  "Use the name of FILE to determine the title of the corresponding help topic."
  (->> file
    (file-name-nondirectory)
    (file-name-sans-extension)))

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
             (topics (-remove 'null (mapcar 'sclang--filename->topic files)))
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
        (max-lisp-eval-depth 10000)
        )
    (message "Indexing help topics ...")
    (setq sclang-help-topic-alist
          (sclang-make-help-topic-alist (sclang-help-directories) nil))
    (message "Indexing help topics ... Done")))

(defun sclang--read-help-topic ()
  "Read an SCDoc topic, with a default value."
  (let* ((topic (sclang-symbol-at-point))
         (default (if topic (format " (default %s)" topic) ""))
         (prompt (format "Help topic%s: " default)))
    (completing-read prompt
                     sclang-scdoc-topics nil nil nil
                     'sclang-help-topic-history topic)))

(cl-defun sclang--blocking-eval-string (expr &optional (timeout-ms 100))
  "Ask SuperCollider to evaluate the given string EXPR. Wait a maximum TIMEOUT-MS."
  (let ((result nil)
        (elapsed 0)
        ;; Prevent expressions from crashing sclang.
        (fmt (format "try { Emacs.message((%s).asString) } {|err|}" expr))
        )
    ;; SuperCollider will eval the string and then call back with the result.
    ;; We rebind Emacs' `message' action to intercept the response.
    (flet ((message (str &rest _) (setq result str)))

      (sclang-eval-string fmt)

      ;; Block until we receive a response or the timeout expires.
      (while (and (not result) (> timeout-ms elapsed))
        (sleep-for 0 10)
        (setq elapsed (+ 10 elapsed)))
      result)))

(defun sclang--topic->helpfile (topic)
  "Get the absolute path to the SuperCollider help file for TOPIC."
  (sclang--blocking-eval-string
   (format "SCDoc.findHelpFile(\"%s\")" topic)))

(defun sclang--sc-help-buffer? (buf)
  "Non-nil if buffer BUF is a SuperCollider help buffer."
  (with-current-buffer buf
    (equal (buffer-name) sclang--help-buffer-name)))

(defun sclang--help-buffer? (buf)
  "Non-nil if buffer BUF is an Emacs help buffer."
  (with-current-buffer buf (equal major-mode 'help-mode)))

(defun sclang--help-window-visible? ()
  "Non-nil if the SCLang w3m help window is on-screen."
  (->> (window-list)
    (-map 'window-buffer)
    (--any? (or (sclang--sc-help-buffer? it)
                (sclang--help-buffer? it)))))

(defun sclang--switch-to-help-window ()
  "Switch to an open help window."
  (when (sclang--help-window-visible?)
    (while (not (or (sclang--sc-help-buffer? (current-buffer))
                    (sclang--help-buffer? (current-buffer))))
      (other-window 1))
    (current-buffer)))

(defun sclang--open-help-window (file)
  "Open the help window and navigate to FILE."
  ;; Switch to a help window if it is open. Otherwise, split the window.
  (or (sclang--switch-to-help-window)
      (progn (split-window) (other-window +1)))

  ;; Open the help page in the current window.
  (condition-case _err
      (w3m-browse-url file)
    (error
     (error "No help for \"%s\"" (sclang--filename->topic file))))

  ;; Rename buffer if the help buffer was successfully opened.
  (if (equal major-mode 'w3m-mode)
      (rename-buffer sclang--help-buffer-name)
    (error "SuperCollider help system not initialized")))

(defadvice w3m-close-window (around delete-sc-help-window activate)
  "Delete the SuperCollider help window, rather than killing w3m."
  (if (and (equal (buffer-name) sclang--help-buffer-name)
           (< 1 (length (window-list))))
      (delete-window)
    ad-do-it))

(defun sclang-find-help (topic)
  "Prompt the user for a help TOPIC and display the topic in a browser window."
  (interactive (list (sclang--read-help-topic)))
  (sclang--open-help-window (sclang--topic->helpfile topic)))

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

(provide 'sclang-help)

;;; NOTES:
;;; * lexical-binding: Because it's generally the expected binding behaviour.
;;; * not obsolete: We need `flet' for dynamically rebinding functions.

;; Local Variables:
;; lexical-binding: t
;; byte-compile-warnings: (not obsolete)
;; End:

;;; sclang-help.el ends here
