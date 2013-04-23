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
(autoload 'w3m-find-file "w3m")
(autoload 'w3m-copy-buffer "w3m")

;;; ----------------------------------------------------------------------------
;;; Customizable variables..

(defcustom sclang-help-path
  (list sclang-system-help-dir "~/.local/share/SuperCollider/Help")
  "*List of directories where SuperCollider help files are kept."
  :group 'sclang-interface
  :version "21.4"
  :type '(repeat directory))

(defconst sclang-extension-path
  (list sclang-system-extension-dir "~/.local/share/SuperCollider/Extensions")
  "List of SuperCollider extension directories.")

(defcustom sclang-help-fill-column fill-column
  "*Column beyond which automatic line-wrapping in RTF help files should happen."
  :group 'sclang-interface
  :version "21.3"
  :type 'integer)

(defcustom sclang-rtf-editor-program "ted"
  "*Name of an RTF editor program used to edit SuperCollider help files."
  :group 'sclang-programs
  :version "21.3"
  :type 'string)

;;; ----------------------------------------------------------------------------
;;; Help file caching

(defvar sclang-help-topic-alist nil
  "Alist mapping help topics to file names.")

(defvar sclang-help-topic-history nil
  "List of recently invoked help topics.")

(defconst sclang-special-help-topics
  '(("/" . "division")
    ("-" . "subtraction"))
  "Alist of help topics with transcoded filenames.")

(defvar-local sclang-help-file nil
  "Path to a help file associated with a buffer.")

(defvar-local sclang-current-help-file nil
  "Path to a help file corresponding to the current buffer.")

(defun sclang-help-topic-name (file)
  "Use the name of FILE to determine the title of the corresponding help topic."
  (->> file
    (file-name-nondirectory)
    (file-name-sans-extension)))

;;; ----------------------------------------------------------------------------
;;; File-type predicates

(defun sclang-get-help-topic (file)
  "Get the help topic corresponding to FILE."
  (let ((topic (car (rassoc file sclang-help-topic-alist))))
    (or (car (rassoc topic sclang-special-help-topics)) topic)))

(defun sclang-help-buffer-name (topic)
  "Format a buffer name for the given help TOPIC."
  (sclang-make-buffer-name (concat "Help:" topic)))

(defmacro sclang/def-filetype-predicate (name extension)
  "Define a file type predicate to match a certain extension.
* NAME is the name of the function.
* EXTENSION is the file extension to match."
  (declare (indent 1))
  `(defun ,name (file)
     ,(concat "Return non-nil if FILE has extension " extension ".")
     (equal ,extension (downcase (file-name-extension file)))))

(sclang/def-filetype-predicate sclang-html-file? "html")
(sclang/def-filetype-predicate sclang-rtf-file?  "rtf")

;;; ----------------------------------------------------------------------------
;;; RTF parsing

(defconst sclang-rtf-face-change-token "\0")

(defvar sclang-rtf-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry 92 "/" table)
    (modify-syntax-entry 34 "." table)
    (modify-syntax-entry 123 "(" table)
    (modify-syntax-entry 125 ")" table)
    (modify-syntax-entry 40 "." table)
    (modify-syntax-entry 41 "." table)
    (modify-syntax-entry 91 "." table)
    (modify-syntax-entry 93 "." table)
    table)
  "Syntax table used for RTF parsing.")

(defvar sclang-rtf-font-map
  '((Helvetica . variable-pitch)
    (Helvetica-Bold . variable-pitch)
    (Monaco . nil)))

(defstruct sclang-rtf-state
  output font-table font face pos)

(macrolet ((rtf-p (pos) `(plist-get (text-properties-at ,pos) 'rtf-p)))
  (defun sclang-rtf-p (pos) (rtf-p pos))
  (defun sclang-code-p (pos) (not (rtf-p pos))))

(defmacro with-sclang-rtf-state-output (state &rest body)
  `(with-current-buffer (sclang-rtf-state-output ,state)
     ,@body))

(defmacro sclang-rtf-state-add-font (state font-id font-name)
  `(push (cons ,font-id (intern ,font-name)) (sclang-rtf-state-font-table ,state)))

(defmacro sclang-rtf-state-apply (state)
  (let ((pos (cl-gensym))
        (font (cl-gensym))
        (face (cl-gensym)))
    `(with-current-buffer (sclang-rtf-state-output ,state)
       (let ((,pos (or (sclang-rtf-state-pos ,state) (point-min)))
             (,font (cdr (assq
                          (cdr (assoc
                                (sclang-rtf-state-font ,state)
                                (sclang-rtf-state-font-table ,state)))
                          sclang-rtf-font-map)))
             (,face (sclang-rtf-state-face ,state)))
         (when (> (point) ,pos)
           (if ,font
               (add-text-properties
                ,pos (point)
                (list 'rtf-p t 'rtf-face (append (list ,font) ,face))))
           (setf (sclang-rtf-state-pos ,state) (point)))))))

(defmacro sclang-rtf-state-set-font (state font)
  `(progn
     (sclang-rtf-state-apply ,state)
     (setf (sclang-rtf-state-font ,state) ,font)))

(defmacro sclang-rtf-state-push-face (state face)
  (let ((list (cl-gensym)))
    `(let ((,list (sclang-rtf-state-face state)))
       (sclang-rtf-state-apply ,state)
       (unless (memq ,face ,list)
         (setf (sclang-rtf-state-face ,state)
               (append ,list (list ,face)))))))

(defmacro sclang-rtf-state-pop-face (state face)
  (let ((list (cl-gensym)))
    `(let* ((,list (sclang-rtf-state-face ,state)))
       (sclang-rtf-state-apply ,state)
       (setf (sclang-rtf-state-face ,state) (delq ,face ,list)))))

(defun sclang-parse-rtf (state)
  (while (not (eobp))
    (cond ((looking-at "{")
           ;; container
           (let ((beg (point)))
             (with-syntax-table sclang-rtf-syntax-table
               (forward-list 1))
             (save-excursion
               (save-restriction
                 (narrow-to-region (1+ beg) (1- (point)))
                 (goto-char (point-min))
                 (sclang-parse-rtf-container state)
                 (widen)))))
          ((or (looking-at "\\\\\\([{}\\\n]\\)")
               (looking-at "\\\\\\([^\\ \n]+\\) ?"))
           ;; control
           (let ((end (match-end 0)))
             (sclang-parse-rtf-control state (match-string 1))
             (goto-char end)))
          ((looking-at "\\([^{\\\n]+\\)")
           ;; normal text
           (let ((end (match-end 0))
                 (match (match-string 1)))
             (with-sclang-rtf-state-output state (insert match))
             (goto-char end)))
          (t
           ;; never reached (?)
           (forward-char 1)))))

(defun sclang-parse-rtf-container (state)
  (cond ((looking-at "\\\\rtf1")		; document
         (goto-char (match-end 0))
         (sclang-parse-rtf state))
        ((looking-at "\\\\fonttbl")		; font table
         (goto-char (match-end 0))
         (while (looking-at "\\\\\\(f[0-9]+\\)[^ ]* \\([^;]*\\);[^\\]*")
           (sclang-rtf-state-add-font state (match-string 1) (match-string 2))
           (goto-char (match-end 0))))
        ((looking-at "{\\\\NeXTGraphic \\([^\\]+\\.[a-z]+\\)") ; inline graphic
         (let* ((file (match-string 1))
                (image (and file (create-image (expand-file-name file)))))
           (with-sclang-rtf-state-output
            state
            (if image
                (insert-image image)
              (sclang-rtf-state-push-face state 'italic)
              (insert file)
              (sclang-rtf-state-pop-face state 'italic)))))
        ))

(defun sclang-parse-rtf-control (state ctrl)
  (let ((char (aref ctrl 0)))
    (cond ((memq char '(?{ ?} ?\\))
           (with-sclang-rtf-state-output state (insert char)))
          ((or (eq char ?\n)
               (string= ctrl "par"))
           (sclang-rtf-state-apply state)
           (with-sclang-rtf-state-output
            state
            (when (sclang-rtf-p (line-beginning-position))
              (fill-region (line-beginning-position) (line-end-position)
                           t t))
              (insert ?\n)))
          ((string= ctrl "tab")
           (with-sclang-rtf-state-output state (insert ?\t)))
          ((string= ctrl "b")
           (sclang-rtf-state-push-face state 'bold))
          ((string= ctrl "b0")
           (sclang-rtf-state-pop-face state 'bold))
          ((string-match "^f[0-9]+$" ctrl)
           (sclang-rtf-state-set-font state ctrl))
          )))

(defun sclang-convert-rtf-buffer (output)
  (let ((case-fold-search nil)
        (fill-column sclang-help-fill-column))
    (save-excursion
      (goto-char (point-min))
      (when (looking-at "{\\\\rtf1")
        (let ((state (make-sclang-rtf-state)))
          (setf (sclang-rtf-state-output state) output)
          (sclang-parse-rtf state)
          (sclang-rtf-state-apply state))))))

;; =====================================================================
;; help mode
;; =====================================================================

(defun sclang-fill-help-syntax-table (table)
  ;; make ?- be part of symbols for selection and sclang-symbol-at-point
  (modify-syntax-entry ?- "_" table))

(defun sclang-fill-help-mode-map (map)
  (define-key map "\C-c}" 'bury-buffer)
  (define-key map "\C-c\C-v" 'sclang-edit-help-file))

(defmacro sclang-help-mode-limit-point-to-code (&rest body)
  (let ((min (cl-gensym))
        (max (cl-gensym))
        (res (cl-gensym)))
    `(if (and (sclang-code-p (point))
              (not (or (bobp) (eobp)))
              (sclang-code-p (1- (point)))
              (sclang-code-p (1+ (point))))
         (let ((,min (previous-single-property-change (point) 'rtf-p (current-buffer) (point-min)))
               (,max (next-single-property-change (point) 'rtf-p (current-buffer) (point-max))))
           (let ((,res (progn ,@body)))
             (cond ((< (point) ,min) (goto-char ,min) nil)
                   ((> (point) ,max) (goto-char ,max) nil)
                   (t ,res)))))))

(defun sclang-help-mode-beginning-of-defun (&optional arg)
  (interactive "p")
  (sclang-help-mode-limit-point-to-code (sclang-beginning-of-defun arg)))

(defun sclang-help-mode-end-of-defun (&optional arg)
  (interactive "p")
  (sclang-help-mode-limit-point-to-code (sclang-end-of-defun arg)))

(defun sclang-help-mode-fontify-region (start end loudly)
  (flet ((fontify-code
          (start end loudly)
          (funcall 'font-lock-default-fontify-region start end loudly))
         (fontify-non-code
          (start end loudly)
          (while (< start end)
            (let ((value (plist-get (text-properties-at start) 'rtf-face))
                  (end (next-single-property-change start 'rtf-face (current-buffer) end)))
                (add-text-properties start end (list 'face (append '(variable-pitch) (list value))))
                (setq start end)))))
    (let ((modified (buffer-modified-p)) (buffer-undo-list t)
          (inhibit-read-only t) (inhibit-point-motion-hooks t)
          (inhibit-modification-hooks t)
          deactivate-mark buffer-file-name buffer-file-truename
          (pos start))
      (unwind-protect
          (while (< pos end)
            (let ((end (next-single-property-change pos 'rtf-p (current-buffer) end)))
              (if (sclang-rtf-p pos)
                  (fontify-non-code pos end loudly)
                (fontify-code pos end loudly))
              (setq pos end)))
        (when (and (not modified) (buffer-modified-p))
          (set-buffer-modified-p nil))))))

(defun sclang-help-mode-indent-line ()
  (if (sclang-code-p (point))
      (sclang-indent-line)
    (insert "\t")))

(define-derived-mode sclang-help-mode sclang-mode "SCLangHelp"
  "Major mode for displaying SuperCollider help files.
\\{sclang-help-mode-map}"
  (let ((file (or (buffer-file-name)
                  (and (boundp 'sclang-current-help-file)
                       sclang-current-help-file))))
    (when file
      (set-visited-file-name nil)
      (setq buffer-auto-save-file-name nil)
      (save-excursion
        (when (sclang-rtf-file? file)
          (let ((tmp-buffer (generate-new-buffer " *RTF*"))
                (modified-p (buffer-modified-p)))
            (unwind-protect
                (progn
                  (sclang-convert-rtf-buffer tmp-buffer)
                  (read-only-mode -1)
                  (erase-buffer)
                  (insert-buffer-substring tmp-buffer))
              (and (buffer-modified-p) (not modified-p) (set-buffer-modified-p nil))
              (kill-buffer tmp-buffer))))))
    (set (make-local-variable 'sclang-help-file) file)
    (setq font-lock-defaults
          (append font-lock-defaults
                  '((font-lock-fontify-region-function . sclang-help-mode-fontify-region))))
    (set (make-local-variable 'beginning-of-defun-function) 'sclang-help-mode-beginning-of-defun)
    (set (make-local-variable 'indent-line-function) 'sclang-help-mode-indent-line)
    ))

;; =====================================================================
;; help file access
;; =====================================================================

(defun sclang-skip-help-directory-p (path)
  "Answer t if PATH should be skipped during help file indexing."
  (let ((directory (file-name-nondirectory path)))
    (-reduce (lambda (a b) (or a b))
            (mapcar (lambda (regexp) (string-match regexp directory))
                    '("^\.$" "^\.\.$" "^CVS$" "^\.svn$" "^_darcs$")))))

(defun sclang-filter-help-directories (list)
  "Remove paths to be skipped from LIST of directories."
  (--remove (or (not (file-directory-p it)) (sclang-skip-help-directory-p it))
             list))

(defun sclang-directory-files-save (directory &optional full match nosort)
  "Return a list of names of files in DIRECTORY, or nil on error."
  (condition-case nil
      (directory-files directory full match nosort)
    (error nil)))

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

(defun sclang-edit-html-help-file ()
  "Edit the help file associated with the current buffer.
Switches w3m to edit mode (actually HTML mode)."
  (interactive)
  (w3m-edit-current-url))

(defun sclang-edit-help-code ()
  "Edit the help file to make code variations.
Switches to text mode with sclang-minor-mode."
  (interactive)
  (w3m-copy-buffer)
  (sclang-mode)
  (read-only-mode +1)
  (rename-buffer "*SC_Help:CodeEdit*"))

(defun sclang-edit-help-file ()
  "Edit the help file associated with the current buffer.
Either visit file internally (.sc) or start external editor (.rtf)."
  (interactive)
  (if (and (boundp 'sclang-help-file) sclang-help-file)
      (let ((file sclang-help-file))
        (if (file-exists-p file)
            (if (sclang-rtf-file? file)
                (start-process (sclang-make-buffer-name (format "HelpEditor:%s" file))
                               nil sclang-rtf-editor-program file)
              (find-file file))
          (if (sclang-html-file? file)
              (w3m-edit-current-url))
          (sclang-message "Help file not found")))
    (sclang-message "Buffer has no associated help file")))

(defun sclang-help-topic-at-point ()
  "Answer the help topic at point, or nil if not found."
  (save-excursion
    (with-syntax-table sclang-help-mode-syntax-table
      (let (beg end)
        (skip-syntax-backward "w_")
        (setq beg (point))
        (skip-syntax-forward "w_")
        (setq end (point))
        (goto-char beg)
        (car (assoc (buffer-substring-no-properties beg end)
                    sclang-help-topic-alist))))))

(defun sclang-goto-help-browser ()
  "Switch to the *w3m* buffer to browse help files."
  (interactive)
  (let ((buf (or (get-buffer "*w3m*")
                 (get-buffer "*SC_Help:w3m*")
                 (sclang-find-help "Help"))))
    (when buf
      (switch-to-buffer buf)
      (with-current-buffer buf
        (rename-buffer "*SC_Help:w3m*")
        (sclang-help-minor-mode)))))

(defun sclang--create-help-buffer (file bufname)
  "Create a help buffer for a non-SClang help file."
  (with-current-buffer (get-buffer-create bufname)
    (let ((sclang-current-help-file file)
          (default-directory (file-name-directory file)))
      (insert-file-contents file)
      (sclang-help-mode)
      (set-buffer-modified-p nil)
      (current-buffer))))

(defun sclang--find-help-file (file)
  "Find the help file corresponding with FILE."
  (let* ((topic (sclang-get-help-topic file))
         (bufname (sclang-help-buffer-name topic))
         (buf (or (get-buffer bufname)
                  (if (sclang-html-file? file)
                      (w3m-find-file file)
                    (sclang--create-help-buffer file bufname))))
         )
    (switch-to-buffer buf)
    (when (sclang-html-file? file)
      (sclang-goto-help-browser))))

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
    (flet ((message (str &rest args) (setq result str)))
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
    (message "TOPIC: %s, FILE: %s" topic file)
    (if file
        (w3m-find-file file)
      (error "No help for \"%s\"" topic)) nil))

(defun sclang-open-help-gui ()
  "Open SCDoc Help Browser."
  (interactive)
  (sclang-eval-string (sclang-format "Help.gui")))

(defvar sclang-scdoc-topics (make-hash-table :size 16385)
  "List of all scdoc topics.")

(sclang-set-command-handler
 'helpSymbols
 (lambda (syms)
   (--map (puthash it nil sclang-scdoc-topics) syms)))

(defun sclang-find-help-in-gui (topic)
  "Search for TOPIC in SCDoc Help Browser."
  (interactive (list (sclang--read-help-topic)))
  (if topic
      (sclang-eval-string (sclang-format "HelpBrowser.openHelpFor(%o)" topic))
    (sclang-eval-string (sclang-format "Help.gui"))))

;; =====================================================================
;; module setup
;; =====================================================================

(add-hook 'sclang-library-startup-hook
          (lambda ()
            (sclang-perform-command 'helpSymbols)
            (ignore-errors (sclang-index-help-topics))))

(add-hook 'sclang-library-shutdown-hook (lambda () (clrhash sclang-scdoc-topics)))

(add-to-list 'auto-mode-alist '("\\.rtf$" . sclang-help-mode))

(sclang-fill-help-syntax-table sclang-help-mode-syntax-table)
(sclang-fill-help-mode-map sclang-help-mode-map)

(provide 'sclang-help)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; sclang-help.el ends here
