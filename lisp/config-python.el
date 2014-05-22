;;; config-python.el --- Configure python

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

;; Configure python

;;; Code:

(require 'utils-common)
(require 'utils-buffers)
(require 'config-evil)
(require 'config-modegroups)
(require 'config-smartparens)
(require 'super-smart-ops)

(cb:declare-package-installer python
  :match "\\.py"
  :packages (python
             python-info
             pyvenv
             virtualenvwrapper))


(custom-set-variables
 '(ropemacs-use-pop-to-buffer t)
 '(ropemacs-guess-project t))

(hook-fn 'python-mode-hook
  (run-hooks 'prog-mode-hook))

(after 'python
  (require 'pyvenv)
  (require 'virtualenvwrapper)

  (define-key python-mode-map (kbd ",") 'cb:comma-then-space)
  (define-key inferior-python-mode-map (kbd ",") 'cb:comma-then-space)

  (after 'evil
    (define-evil-doc-handler cb:python-modes
      (call-interactively 'rope-show-doc)))

  (defun cb-py:restart-python ()
    (save-window-excursion
      (let (kill-buffer-query-functions
            (buf (get-buffer "*Python*")))
        (when buf (kill-buffer buf)))
      (call-interactively 'run-python)))

  (defun cb:switch-to-python ()
    "Switch to the last active Python buffer."
    (interactive)
    ;; Start inferior python if necessary.
    (unless (->> (--first-buffer (derived-mode-p 'inferior-python-mode))
              (get-buffer-process)
              (processp))
      (cb-py:restart-python))

    (if (derived-mode-p 'inferior-python-mode)
        ;; Switch from inferior python to source file.
        (switch-to-buffer-other-window
         (--first-buffer (derived-mode-p 'python-mode)))
      ;; Switch from source file to REPL.
      ;; HACK: `switch-to-buffer-other-window' does not change window
      ;; when switching to REPL buffer. Work around this.
      (-when-let* ((buf (--first-buffer (derived-mode-p 'inferior-python-mode)))
                   (win (or (--first-window (equal (get-buffer "*Python*")
                                                   (window-buffer it)))
                            (split-window-sensibly)
                            (next-window))))
        (set-window-buffer win buf)
        (select-window win)
        (goto-char (point-max))
        (cb:maybe-evil-append-line 1))))

  (define-key python-mode-map (kbd "C-c C-z") 'cb:switch-to-python)
  (define-key inferior-python-mode-map (kbd "C-c C-z") 'cb:switch-to-python)

  (defun cb-py:eval-dwim (&optional arg)
    (interactive "P")
    (cond
     ((region-active-p)
      (python-shell-send-region (region-beginning) (region-end))
      (deactivate-mark))
     (t
      (python-shell-send-defun arg))))

  (define-key python-mode-map (kbd "C-c C-c") 'cb-py:eval-dwim)

  (defun cb-py:smart-equals ()
    "Insert an '=' char padded by spaces, except in function arglists."
    (interactive)
    (if (s-matches? (rx (* space) "def" space) (current-line))
        (insert "=")
      (smart-insert-op "=")))

  (defun cb-py:smart-asterisk ()
    "Insert an asterisk with padding unless we're in an arglist."
    (interactive "*")
    (cond
     ((s-matches? (rx (* space) "def" space) (current-line))
      (insert "*"))
     ;; Collapse whitespace around exponentiation operator.
     ((thing-at-point-looking-at (rx (* space) "*" (* space)))
      (delete-horizontal-space)
      (save-excursion
        (search-backward "*")
        (delete-horizontal-space))
      (insert "*"))
     (t
      (smart-insert-op "*"))))

  (defun cb-py:smart-comma ()
    "Insert a comma with padding."
    (interactive "*")
    (insert ",")
    (just-one-space))

  (defun cb-py:smart-colon ()
    "Insert a colon with padding."
    (interactive "*")
    (insert ":")
    (just-one-space))

  (--each '(python-mode inferior-python-mode)
    (declare-smart-ops it
      :add '("?" "$")
      :custom
      '(("," . cb-py:smart-comma)
        ("*" . cb-py:smart-asterisk)
        (":" . cb-py:smart-colon)
        ("=" . cb-py:smart-equals))))

  (sp-with-modes cb:python-modes
    (sp-local-pair "{" "}" :post-handlers '(:add sp-generic-leading-space)))

  (after 'autoinsert
    (define-auto-insert
      '("\\.py$" . "Python skeleton")
      '("Short description: "
        "\"\"\"\n"
        str
        "\n\"\"\"\n\n"
        _
        "\n")))

  (defun cb-py:split-arglist (arglist)
    "Parse ARGLIST into a list of parameters.
Each element is either a string or a cons of (var . default)."
    (cl-loop
     for arg in (s-split (rx ",") arglist t)
     for (x . y)  = (s-split "=" arg)
     for (_ name) = (s-match (rx (* (any "*")) (group (* (any "_" alnum)))) x)
     for default  = (when y (car y))
     when (not (s-blank? (s-trim name)))
     collect (if default (cons name default) name)))

  (defun cb-py:python-docstring (arglist)
    "Format a docstring according to ARGLIST."
    (let ((al (s-replace " " "" arglist)))
      (if (s-blank? al)
          ""
        (cl-destructuring-bind (keywords formal)
            (-separate 'listp (cb-py:split-arglist al))
          (concat
           (when (or formal keywords) "\n")
           ;; Formal args
           (when (and formal keywords) "    Formal arguments:\n")
           (s-join "\n" (--map (format "    %s --" it) formal))
           (when keywords "\n\n")
           ;; Keyword args
           (when (and formal keywords) "    Keyword arguments:\n")
           (s-join "\n" (--map (format "    %s (default %s) --" (car it) (cdr it))
                               keywords)))))))

  (defun cb-py:arglist-for-function-at-point ()
    "Return the arglist for the function at point, or nil if none."
    (save-excursion
      (when (beginning-of-defun)
        (let ((start (search-forward "("))
              (end (1- (search-forward ")"))))
          (buffer-substring start end)))))

  (defun cb-py:insert-docstring ()
    "Insert a docstring for the python function at point."
    (interactive "*")
    (-when-let (arglist (cb-py:arglist-for-function-at-point))
      (when (beginning-of-defun)
        (search-forward-regexp (rx ":" (* space) eol))
        (newline)
        (open-line 1)
        (insert (concat "    \"\"\"\n"
                        (cb-py:python-docstring arglist) "\n\n"
                        "    Returns:\n\n"
                        "    \"\"\"" ))
        (message "Arglist inserted."))))

  (add-to-list 'insertion-picker-options
               '("d" "Docstring" cb-py:insert-docstring :modes (python-mode)))

  (define-key python-mode-map (kbd "M-q") 'indent-dwim)

  (venv-initialize-eshell)

  (when (fboundp 'ropemacs-mode)
    (add-hook 'python-mode-hook 'ropemacs-mode))

  (after 'rope
    (define-key python-mode-map (kbd "M-.") 'rope-goto-definition))

  (after 'evil
    (evil-define-key 'normal python-mode-map (kbd "M-.") 'rope-goto-definition))

  )


(provide 'config-python)

;;; config-python.el ends here
