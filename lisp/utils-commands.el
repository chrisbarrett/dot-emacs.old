;;; utils-commands.el --- Random interactive commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Chris Barrett

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

;; Random interactive commands

;;; Code:

(require 'utils-common)
(require 'utils-buffers)
(require 'utils-shell)
(require 'config-modegroups)

;;; Autoloads

(autoload 'org-move-item-down "org-list")
(autoload 'org-move-item-up "org-list")
(autoload 'sp-beginning-of-sexp "smartparens")
(autoload 'sp-get-enclosing-sexp "smartparens")
(autoload 'thing-at-point-looking-at "thingatpt")

;;; Exiting Emacs

(defun cb:exit-emacs ()
  (interactive)
  (when (yes-or-no-p "Kill Emacs? ")
    (save-buffers-kill-emacs)))

(defun cb:exit-emacs-dwim ()
  (interactive)
  (when (yes-or-no-p "Kill Emacs? ")
    (if (daemonp)
        (server-save-buffers-kill-terminal nil)
      (save-buffers-kill-emacs))))

;;; Evil compatibility

(defun cb:maybe-evil-insert-state ()
  "Only enter insert state if evil-mode is active."
  (interactive)
  (when (and (fboundp 'evil-insert-state)
             (true? evil-mode))
    (evil-insert-state)))

(defun cb:maybe-evil-append-line (&rest _)
  "Go to the end of the line. Enter insert state if evil-mode is active."
  (if (and (fboundp 'evil-append-line)
           (true? evil-mode))
      (evil-append-line 1)
    (end-of-line)))

(defun cb:append-buffer ()
  "Enter insertion mode at the end of the current buffer."
  (interactive)
  (goto-char (point-max))
  (cb:maybe-evil-insert-state))

;;; Create indirect buffer from region.

(defvar-local indirect-mode-name nil
  "Mode to set for indirect buffers.")

(defun indirect-region (start end)
  "Edit the current region from START to END in another buffer.
If the buffer-local variable `indirect-mode-name' is not set, prompt
for mode name to choose for the indirect buffer interactively.
Otherwise, use the value of said variable as argument to a funcall."
  (interactive "r")
  (let ((buffer-name (generate-new-buffer-name "*indirect*"))
        (mode
         (if (not indirect-mode-name)
             (setq indirect-mode-name
                   (intern
                    (completing-read
                     "Mode: "
                     (--map (list (symbol-name it))
                            (apropos-internal "-mode$" 'commandp))
                     nil t)))
           indirect-mode-name)))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (funcall mode)
    (narrow-to-region start end)
    (goto-char (point-min))
    (shrink-window-if-larger-than-buffer)))

;;; Indentation

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (ignore-errors
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (indent-for-tab-command)
        (forward-line)))))

(defun indent-dwim (&optional arg)
  "Perform a context-sensitive indentation action.
With prefix argument ARG, justify text."
  (interactive "P")
  (let ((in-string? (nth 8 (syntax-ppss))))
    (cond
     ((region-active-p)
      (indent-region (region-beginning) (region-end))
      (message "Indented region."))

     (in-string?
      (if (apply 'derived-mode-p cb:lisp-modes)
          (lisp-fill-paragraph arg)
        (or (fill-comment-paragraph)
            (fill-paragraph arg)))
      (message "Filled paragraph."))

     (t
      (indent-buffer)
      (message "Indented buffer.")))))

(defun outdent ()
  "Remove indentation on the current line."
  (interactive "*")
  (save-excursion
    (goto-char (line-beginning-position))
    (delete-horizontal-space)))

;;; Buffer management.

(defun delete-buffer-and-file ()
  "Delete a file and its associated buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defalias 'delete-file-and-buffer 'delete-buffer-and-file)

(defun rename-buffer-and-file ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))

(defalias 'rename-file-and-buffer 'rename-buffer-and-file)

(defvar cb:kill-buffer-ignored-list
  '("*scratch*" "*Messages*" "*Group*"
    "*shell*" "*eshell*" "*ansi-term*"
    "diary.org" "notes.org"))

(defun kill-current-buffer ()
  "Kill the current buffer.
If this buffer is a member of `cb:kill-buffer-ignored-list, bury it rather than killing it."
  (interactive)
  (if (member (buffer-name (current-buffer)) cb:kill-buffer-ignored-list)
      (bury-buffer)
    (kill-buffer (current-buffer))))

(defun clean-buffers ()
  "Close all buffers not in the ignore list."
  (interactive)
  (delete-other-windows)
  (-each (--filter-buffers
          (not (or (-contains? cb:kill-buffer-ignored-list (buffer-name it))
                   (get-buffer-process it))))
    'kill-buffer))

(defun expose-buffers-by-mode (&optional mode arg)
  "Show all buffers with major mode MODE.
With a prefix ARG, show all buffers"
  (interactive (list
                (->> (--filter-buffers
                      (and (derived-mode-p 'prog-mode 'text-mode)
                           (or current-prefix-arg (buffer-file-name))))
                  (--map-buffers (symbol-name major-mode))
                  (-sort 'string<)
                  (-uniq)
                  (ido-completing-read "Mode: ")
                  (intern))

                current-prefix-arg))
  (expose-buffers (--filter-buffers (and (derived-mode-p mode)
                                         (or arg (buffer-file-name))))))

(defun cb:rotate-buffers ()
  "Rotate active buffers, retaining the window layout.
Changes the selected buffer."
  (interactive)
  ;; Bail if there are not enough windows to rotate.
  (unless (> (count-windows) 1)
    (user-error "Cannot rotate single window"))
  ;; Perform rotation.
  (let ((i 1)
        (n-windows (count-windows)))
    (while  (< i n-windows)
      (let* (
             (w1 (elt (window-list) i))
             (w2 (elt (window-list) (+ (% i n-windows) 1)))
             (b1 (window-buffer w1))
             (b2 (window-buffer w2))
             (s1 (window-start w1))
             (s2 (window-start w2))
             )
        (set-window-buffer w1  b2)
        (set-window-buffer w2 b1)
        (set-window-start w1 s2)
        (set-window-start w2 s1)
        (setq i (1+ i))))))

;;; Display autoloaded functions in buffer.

(defun cb:find-autoloads (buffer)
  (->> (with-current-buffer buffer
         (buffer-substring-no-properties (point-min) (point-max)))
    (s-match-strings-all (rx ";;;###autoload" "\n"
                             (* space) "("(+ (not space)) (+ space) (? "'")
                             (group (+ (not space)))))
    (-map 'cadr)))

(cl-defun show-autoloads (&optional (buffer (current-buffer)))
  "Find the autoloaded definitions in BUFFER"
  (interactive)
  (-if-let (results (-map (~ s-append "\n") (cb:find-autoloads buffer)))
      (with-output-to-temp-buffer "*autoloads*"
        (-each results 'princ))

    (error "No autoloads found in current buffer")))

;;; Rewrite `setq' as `defcustom'.

(defun combine-adjacent-setqs ()
  "Combine usages of setq around point."
  (interactive)
  (sp-beginning-of-sexp)
  (when (thing-at-point-looking-at "setq")
    (sp-backward-up-sexp)

    ;; Move to start of group.
    (let ((prev (point)))
      (while (thing-at-point-looking-at "(setq")
        (setq prev (point))
        (sp-backward-sexp))
      (goto-char prev))

    ;; Find number of consecutive setq forms.
    (let ((count 0))
      (save-excursion
        (while (thing-at-point-looking-at "(setq")
          (sp-next-sexp)
          (cl-incf count)))

      ;; Wrap group in another setq.
      (sp-wrap-with-pair "(")
      (insert "setq ")
      (sp-forward-slurp-sexp (1- count))

      ;; Kill interior setqs.
      (--dotimes count
        (sp-forward-symbol)
        (sp-splice-sexp-killing-backward 1)
        (sp-next-sexp 3)))))

(defun rewrite-setq-as-custom-set-variables ()
  "Rewrite the `setq' at point to a usage of `custom-set-variables'."
  (interactive "*")
  (cl-flet ((do-rewrite
             ()
             (combine-adjacent-setqs)
             (forward-char 1)
             (cl-destructuring-bind (&key beg end &allow-other-keys)
                 (sp-get-enclosing-sexp)
               (let* ((form (read (buffer-substring beg end)))
                      (updated
                       (pp-to-string
                        (cons 'custom-set-variables
                              (->> (cdr form)
                                (-partition-in-steps 2 2)
                                (--map (cons 'quote (list it))))))))
                 (goto-char beg)
                 (delete-region beg end)
                 (insert updated)))))
    (cond
     ((thing-at-point-looking-at "(setq")
      (forward-char 1)
      (do-rewrite))
     ((and (sp-beginning-of-sexp)
           (thing-at-point-looking-at "setq"))
      (do-rewrite))
     (t
      (user-error "Not at a setq form")))))

;;; Misc interactive commands

(defun move-line-up ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-up)

    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode)))

(defun move-line-down ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-down)

    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode)))

(defun generate-password (length)
  "Generate a password with a given LENGTH."
  (interactive (list (read-number "Password length: " 32)))
  (let ((pass
         (--> (%-string "openssl" "rand" "-base64" (number-to-string length))
           ;; The encoding process will pad with '=' characters to reach a
           ;; length divisible by 4 bytes. Drop this padding.
           (substring it 0 length))))
    (cond
     ((called-interactively-p 'any)
      (kill-new pass)
      (message "Password copied to kill ring."))
     (t
      pass))))

(defun cb:comma-then-space ()
  (interactive)
  (atomic-change-group
    (insert-char ?\,)
    (just-one-space)))

;;; Key bindings

(define-key prog-mode-map (kbd "M-q") 'indent-dwim)
(global-set-key (kbd "<backtab>") 'outdent)

(bind-key  "C-c C"            'indirect-region)
(bind-key* "C-<down>"         'move-line-down)
(bind-key* "C-<up>"           'move-line-up)
(bind-key* "C-c k b"          'clean-buffers)
(bind-key* "C-c k e"          'cb:exit-emacs)
(bind-key* "C-c k k"          'cb:exit-emacs-dwim)
(bind-key* "C-x <backspace>"  'kill-current-buffer)
(bind-key* "C-x C-c" (command (message "Type <C-c k k> to exit Emacs")))

(provide 'utils-commands)

;;; utils-commands.el ends here
