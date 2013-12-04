;;; cb-python.el --- Configuration for python

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130526.2358

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

;; Configuration for python

;;; Code:

(require 'use-package)
(require 'cb-foundation)
(require 'cb-mode-groups)

;; Enable auto-complete in python modes.
(after 'auto-complete
  (-each cb:python-modes (~ add-to-list 'ac-modes)))

;; Add special smart-operator behaviours for python buffers.
(defun cb-py:smart-equals ()
  "Insert an '=' char padded by spaces, except in function arglists."
  (interactive)
  (if (s-matches? (rx (* space) "def" space) (current-line))
      (insert "=")
    (smart-insert-operator "=")))

(defun cb-py:smart-asterisk ()
  "Insert an asterisk with padding unless we're in an arglist."
  (interactive "*")
  (if (s-matches? (rx (* space) "def" space) (current-line))
      (insert "*")
    (smart-insert-operator "*")))

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

(after 'python
  (define-keys python-mode-map
    "," 'cb-py:smart-comma
    "&" (command (smart-insert-operator "&"))
    "%" (command (smart-insert-operator "%"))
    "?" (command (smart-insert-operator "?"))
    "*" 'cb-py:smart-asterisk
    ":" 'cb-py:smart-colon
    "+" (command (smart-insert-operator "+"))
    "/" (command (smart-insert-operator "/"))
    "-" (command (smart-insert-operator "-"))
    "=" 'cb-py:smart-equals
    "<" (command (smart-insert-operator "<"))
    ">" (command (smart-insert-operator ">"))
    "|" (command (smart-insert-operator "|"))
    "$" (command (smart-insert-operator "$"))))

;; Configure smartparens formatting for python.
(after 'smartparens
  (sp-with-modes cb:python-modes
    (sp-local-pair "{" "}" :post-handlers '(:add sp-generic-leading-space))))

;; Add documentation lookup handler for python.
(after 'cb-evil
  (hook-fn 'evil-find-doc-hook
    (when (apply 'derived-mode-p cb:python-modes)
      (jedi:show-doc)
      t)))

;; Auto-insert header in python files.
(after 'autoinsert
  (define-auto-insert
    '("\\.py$" . "Python skeleton")
    '("Short description: "
      "\"\"\"\n"
      str
      "\n\"\"\"\n\n"
      _
      "\n")))

;; Add functions for manipulating docstrings.

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

;; Add command to insert docstring for fn at point.

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

;; Use `python', the newer package off Marmalade.
(use-package python
  :ensure   t
  :commands python-mode
  :mode     ("\\.py$" . python-mode)
  :init
  (hook-fn 'python-mode-hook
    ;; Python-mode is not derived from prog mode, but we still want all the
    ;; programming goodies.
    (run-hooks 'prog-mode-hook))

  :config
  (progn
    ;; Use ipython if installed.
    (-when-let (ipython (executable-find "ipython"))
      (setq
       python-shell-interpreter ipython
       python-shell-interpreter-args ""
       python-shell-prompt-regexp "In \\[[0-9]+\\]: "
       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
       python-shell-completion-setup-code
       "from IPython.core.completerlib import module_completion"
       python-shell-completion-module-string-code
       "';'.join(module_completion('''%s'''))\n"
       python-shell-completion-string-code
       "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

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
          (when (fboundp 'evil-append-line)
            (evil-append-line 1)))))

    (defun cb-py:eval-dwim (&optional arg)
      (interactive "P")
      (cond
       ((region-active-p)
        (python-shell-send-region (region-beginning) (region-end))
        (deactivate-mark))
       (t
        (python-shell-send-defun arg))))

    (define-keys python-mode-map
      "," 'cb:comma-then-space
      "C-c C-z" 'cb:switch-to-python
      "C-c C-c" 'cb-py:eval-dwim)

    (define-keys inferior-python-mode-map
      "," 'cb:comma-then-space
      "C-c C-z" 'cb:switch-to-python)))

;; Installs an info manual for Python 2.7
(use-package python-info
  :defer t
  :ensure t)

;; `jedi' provides auto-completion, code search and documentation for python
;; buffers.
(use-package jedi
  :ensure   t
  :commands (jedi:setup jedi:show-doc)
  :init     (add-hook 'cb:python-modes-hook 'jedi:setup)
  :config
  (setq jedi:tooltip-method nil
        jedi:key-goto-definition (kbd "M-.")
        jedi:complete-on-dot t
        jedi:install-imenu t))

;; `virtualenv' configures Emacs according to the current python virtualenv
;; settings.
(use-package virtualenv
  :ensure t
  :diminish virtualenv-minor-mode
  :commands (virtualenv-workon
             virtualenv-deactivate
             virtualenv-minor-mode)
  :init
  (after 'python
    (add-hook 'find-file-hook 'virtualenv-minor-mode)))

(provide 'cb-python)

;; Local Variables:
;; End:

;;; cb-python.el ends here
