;;; cb-scala.el --- Configuration for Scala.

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

;; Configuration for Scala.

;;; Code:

(require 'cb-lib)
(require 'use-package)
(require 'thingatpt)
(autoload 'smart-insert-operator "smart-operator")
(autoload 'smart-insert-operator-hook "smart-operator")

;; `scala-mode2' provides support for the Scala language.
(use-package scala-mode2
  :ensure t
  :commands scala-mode
  :config
  (setq scala-indent:align-forms t
        scala-indent:align-parameters t
        scala-indent:default-run-on-strategy scala-indent:eager-strategy))

;; `ensime' adds IDE-like features to scala-mode.
(use-package ensime
  :ensure t
  :disabled t ; doesn't work on my machine. :(
  :commands (ensime ensime-mode)

  ;; Add a command, `configure-ensime', to configure SBT for use with ENSIME. This
  ;; will be run whenever a scala session is started to ensure ensime is ready.

  :init
  (progn
    (defun configure-ensime ()
      "Configure the ENSIME plugin and initialise this project."
      (interactive)
      (unless (or (true? org-src-mode)
                  (s-starts-with? "*Org" (buffer-name)))
        ;; Load supporting functions.
        (require 'ensime)
        ;; Create ensime file if necessary.
        (when (and (not (cbscala:ensime-config-exists?))
                   (y-or-n-p "Create a .ensime file for this project? "))
          (call-interactively 'create-ensime-file))
        ;; Start ENSIME.
        (let ((ensime-prefer-noninteractive t))
          (unless (ensime-connected-p)
            (ensime))
          (ensime-mode +1))))

    (add-hook 'scala-mode-hook 'configure-ensime))

  :config
  (progn

    (defvar ensime-version "0.1.2")

    (defun cbscala:sbt-installed-versions ()
      "Return the sbt versions installed in '~/.sbt'."
      (->> (f-directories "~/.sbt")
        (-filter (C (~ s-matches? (rx (+ (any digit ".")))) f-filename))
        (-map 'f-filename)))

    (defun cbscala:read-installed-sbt-version ()
      (let ((vers (cbscala:sbt-installed-versions)))
        (if (= (length vers) 1)
            (car vers)
          (ido-completing-read "Configure SBT Version: " vers))))

    (defun create-ensime-file (project-root)
      "Interactively create a .ENSIME file at PROJECT-ROOT."
      (interactive (list (or (projectile-project-p)
                             (ido-read-directory-name "Project root: "))))
      (let ((path (f-join project-root ".ensime"))
            (plist (list :root-dir project-root
                         :name
                         (read-string "Project Name: " (f-filename project-root)))))
        (f-write (pp-to-string plist) 'utf-8 path)
        (message "Wrote %s" path)))

    (defun install-ensime (sbt-version)
      "Add ENSIME to the list of global plugins for SBT-VERSION."
      (interactive (list (cbscala:read-installed-sbt-version)))
      (let ((plugins (f-short (f-join "~/.sbt" sbt-version "plugins" "plugins.sbt"))))
        ;; Create plugins dir if necessary.
        (f-mkdir (f-dirname plugins))
        (f-touch plugins)
        ;; Add the ensime plugin.
        (if (s-contains? "org.ensime" (f-read plugins))
            (message "Already configured: %s" plugins)
          (f-append plugins
                    (format "addSbtPlugin(\"org.ensime\" %% \"ensime-sbt-cmd\" %% \"%s\")"
                            ensime-version))
          (message "Updated '%s'. Start sbt and run 'ensime generate' to finish setup."
                   plugins))))

    (defun cbscala:ensime-config-exists? ()
      (-when-let (root (projectile-project-p))
        (f-exists? (f-join root ".ensime"))))))

;; Configure `evil-mode' commands for Scala.
(after '(evil scala-mode2)

  (defun cbscala:join-line ()
    "Adapt `scala-indent:join-line' to behave more like evil's line join.

`scala-indent:join-line' acts like the vanilla `join-line',
joining the current line with the previous one. The vimmy way is
to join the current line with the next.

Try to move to the subsequent line and then join. Then manually move
point to the position of the join."
    (interactive)
    (let (join-pos)
      (save-excursion
        (goto-char (line-end-position))
        (unless (eobp)
          (forward-line)
          (call-interactively 'scala-indent:join-line)
          (setq join-pos (point))))

      (when join-pos
        (goto-char join-pos))))

  (evil-define-key 'normal scala-mode-map
    "J" 'cbscala:join-line))

;; Add ac sources for Scala keywords.
(after 'auto-complete

  (defconst cbscala:scala-keywords
    '("abstract" "case" "catch" "class" "def" "do" "else" "extends" "false" "final"
      "finally" "for" "forSome" "if" "implicit" "import" "lazy" "match" "new" "null"
      "object" "override" "package" "private" "protected" "return" "sealed" "super"
      "this" "throw" "trait" "try" "true" "type" "val" "var" "while" "with" "yield"
      "-" ":" "=" "=>" "<-" "<:" "<%" ">:" "#" "@")
    "List of keywords reserved by the scala language.")

  (ac-define-source scala-keywords
    '((symbol . "k")
      (candidates . cbscala:scala-keywords)
      (action . just-one-space)))

  (add-to-list 'ac-modes 'scala-mode)
  (hook-fn 'ensime-mode-hook
    (setq ac-auto-start 2)
    (-each '(ac-source-yasnippet
             ac-source-scala-keywords
             ac-source-ensime-completions)
           (~ add-to-list 'ac-sources))))

;; Auxiliary functions for Scala snippets.
(after 'yasnippet

  (defun cbscala:find-case-class-parent ()
    (save-excursion
      (if (search-backward-regexp
           (rx (or
                (and bol (* space)
                     (or (and (? "abstract" (+ space)) "class")
                         "trait")
                     (+ space) (group-n 1 (+ alnum)))
                (and bol (* space)
                     "case" (+ space) "class" (* anything) space
                     "extends" (+ space) (group-n 1 (+ alnum)) (* space) eol)))
           nil t)
          (match-string 1)
        ""))))

;; Configure operators for Scala.

(defun cbscala:equals ()
  (interactive)
  (smart-insert-operator "=")
  (just-one-space))

(defun cbscala:colon ()
  (interactive)
  (smart-insert-operator ":")
  (just-one-space))

(defmacro define-scala-variance-op-command (sym op)

  "Define command named SYM to insert a variance operator OP."
  `(defun ,sym ()
     "Insert a variance operator.
Pad in normal expressions. Do not insert padding in variance annotations."
     (interactive "*")
     (cond
      ;; No padding at the start of type parameter.
      ((thing-at-point-looking-at (rx "[" (* space)))
       (delete-horizontal-space)
       (insert ,op))
      ;; Leading padding after a comma, e.g. for a type parameter or function call.
      ((thing-at-point-looking-at (rx "," (* space)))
       (just-one-space)
       (insert ,op))
      ;; Otherwise leading and trailing padding.
      (t
       (smart-insert-operator ,op)))))

(define-scala-variance-op-command cbscala:plus "+")
(define-scala-variance-op-command cbscala:minus "-")

(hook-fn 'scala-mode-hook
  (smart-insert-operator-hook)
  (local-unset-key (kbd "."))
  (bind-keys
    :map scala-mode-map
    "=" 'cbscala:equals
    ":" 'cbscala:colon
    "+" 'cbscala:plus
    "-" 'cbscala:minus))

(provide 'cb-scala)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-scala.el ends here
