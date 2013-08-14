;;; cb-ruby.el --- Ruby configuration

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130526.2349

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

;; Ruby configuration

;;; Code:

(require 'use-package)
(require 'dash)
(require 'cb-lib)
(require 'cb-foundation)

(define-derived-mode erb-mode html-mode
  "ERB" nil
  (when (fboundp 'flycheck-mode)
    (flycheck-mode -1)))

(add-to-list 'auto-mode-alist `("\\.html\\.erb" . erb-mode))

(after 'smart-operator
  (defun cb-rb:smart-colon ()
    "Insert a colon, with or without padding.
If this is the leading colon for a symbol, do not insert padding.
If this is the trailing colon for a hash key, insert padding."
    (interactive)
    (insert ":")
    (when (s-matches? (rx (+ alnum) ":" eol)
                      (buffer-substring (line-beginning-position) (point)))
      (just-one-space)))

  (hook-fn 'cb:ruby-modes-hook
    (local-set-key (kbd ",") (command (insert ",") (just-one-space)))
    (local-set-key (kbd ":") 'cb-rb:smart-colon)
    (local-set-key (kbd "|") (smart-op "|"))
    (local-set-key (kbd "~") (smart-op "~"))
    (local-set-key (kbd "<") (smart-op "<"))
    (local-set-key (kbd ">") (smart-op ">"))
    (local-set-key (kbd "=") (smart-op "="))
    (local-set-key (kbd "-") (smart-op "-"))
    (local-set-key (kbd "+") (smart-op "+"))
    (local-set-key (kbd "/") (smart-op "/"))))

(after 'hideshow
  (add-to-list 'hs-special-modes-alist
               `(ruby-mode
                 ,(rx (or "def" "class" "module" "{" "[")) ; Block start
                 ,(rx (or "}" "]" "end"))                  ; Block end
                 ,(rx (or "#" "=begin"))                   ; Comment start
                 ruby-forward-sexp nil)))

(after 'auto-complete
  (--each cb:ruby-modes
    (add-to-list 'ac-modes it))

  (defun cb-rb:inside-class? ()
    "Test whether point is in the body of a Ruby class."
    (and (not (emr-line-matches? (rx bol (* space) "class" (+ space))))
         (save-excursion
           (sp-backward-up-sexp)
           (thing-at-point-looking-at "class"))))

  (ac-define-source ruby-class-keywords
    '((available  . cb-rb:inside-class?)
      (candidates . '("public" "private" "protected"))
      (symbol     . "s")))

  (ac-define-source ruby-class-attributes
    '((available  . cb-rb:inside-class?)
      (candidates . '("attr_accessor" "attr_reader" "attr_writer"))
      (action     . (lambda () (insert " :")))
      (symbol     . "m")))

  (defun cb-rb:configure-ac ()
    (add-to-list 'ac-sources 'ac-source-yasnippet)
    (add-to-list 'ac-sources 'ac-source-ruby-class-keywords)
    (add-to-list 'ac-sources 'ac-source-ruby-class-attributes))

  (add-hook 'cb:ruby-modes-hook 'cb-rb:configure-ac))

(after 'smartparens
  (require 'smartparens-ruby)

  ;; Change word boundaries so slurping works.
  (after 'ruby-mode
    (modify-syntax-entry ?@ "w" ruby-mode-syntax-table)
    (modify-syntax-entry ?_ "w" ruby-mode-syntax-table)
    (modify-syntax-entry ?! "w" ruby-mode-syntax-table)
    (modify-syntax-entry ?? "w" ruby-mode-syntax-table))

  (defun sp-ruby-should-insert-pipe-close (_id _action _ctx)
    "Test whether to insert the closing pipe for a lambda-binding pipe pair."
    (thing-at-point-looking-at
     (rx-to-string `(and (or "do" "{") (* space) "|"))))

  (defun sp-ruby-sp-hook-space-before (_id action _ctx)
    "Move to point before ID and insert a space."
    (when (equal 'insert action)
      (save-excursion
        (search-backward "|")
        (just-one-space))))

  (defun sp-ruby-sp-hook-space-after (_id action _ctx)
    "Move to point after ID and insert a space."
    (when (equal 'insert action)
      (save-excursion
        (search-forward "|")
        (just-one-space))))

  (sp-with-modes '(ruby-mode inf-ruby-mode)

    (sp-local-pair "{" "}"
                   :post-handlers '(:add sp-generic-leading-space))

    (sp-local-pair "[" "]"
                   :pre-handlers '(sp-ruby-pre-handler)
                   :post-handlers '(:add sp-generic-leading-space))

    (sp-local-pair "%q{" "}" :when '(sp-in-code-p))
    (sp-local-pair "%Q{" "}" :when '(sp-in-code-p))
    (sp-local-pair "%w{" "}" :when '(sp-in-code-p))
    (sp-local-pair "%W{" "}" :when '(sp-in-code-p))
    (sp-local-pair  "%(" ")" :when '(sp-in-code-p))
    (sp-local-pair "%x(" ")" :when '(sp-in-code-p))
    (sp-local-pair  "#{" "}" :when '(sp-in-string-p))

    (sp-local-pair "|" "|"
                   :when '(sp-ruby-should-insert-pipe-close)
                   :unless '(sp-in-string-p)
                   :pre-handlers '(sp-ruby-sp-hook-space-before)
                   :post-handlers '(sp-ruby-sp-hook-space-after))

    (sp-local-pair "case" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :unless '(sp-ruby-in-string-or-word-p)
                   :actions '(insert)
                   :pre-handlers '(sp-ruby-pre-handler)
                   :post-handlers '(sp-ruby-block-post-handler))))

(use-package ruby-mode
  :ensure t
  :mode
  (("\\.rake\\'". ruby-mode)
   ("Rakefile\\'" . ruby-mode)
   ("\\.gemspec\\'" . ruby-mode)
   ("\\.ru\\'" . ruby-mode)
   ("Gemfile\\'" . ruby-mode)
   ("Guardfile\\'" . ruby-mode)
   ("Capfile\\'" . ruby-mode)
   ("\\.thor\\'" . ruby-mode)
   ("Thorfile\\'" . ruby-mode)
   ("Vagrantfile\\'" . ruby-mode)
   ("\\.jbuilder\\'" . ruby-mode))
  :init
  (progn
    (add-to-list 'completion-ignored-extensions ".rbc")
    (hook-fn 'cb:ruby-modes-hook
      (subword-mode +1)))
  :config
  (progn
    (defun cb-rb:rockets->colons ()
      "Convert old-style rockets to new hash literal syntax in the current buffer."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp (rx ":" (group-n 1 (+ (not space)))
                                          (* space)
                                          "=>"
                                          (* space))
                                      nil t)
          (replace-match "\\1: " t nil))))))

(use-package rubocop
  :ensure t
  :commands (rubocop-run-on-project
             rubocop-run-on-directory
             rubocop-run-on-current-file))

(use-package robe
  :ensure t
  :diminish robe-mode
  :defer t
  :commands robe-mode
  :config
  ;; Undefine keys conflicting with org src mode.
  (define-key robe-mode-map (kbd "C-c C-k") nil)
  :init
  (hook-fn 'ruby-mode-hook
    (robe-mode +1)
    (unless (true? inf-ruby-buffer)
      (save-window-excursion
        (inf-ruby)
        (run-with-idle-timer 0.5 nil 'robe-start)))
    (after 'auto-complete
      (add-to-list 'ac-sources 'ac-source-robe))))

(use-package rinari
  :ensure t
  :diminish rinari-minor-mode
  :commands rinari-minor-mode
  :init
  (hook-fn 'cb:rails-modes-hook
    (rinari-minor-mode
     (if (or (true? org-src-mode)
             (s-starts-with? "*Org Src " (buffer-name)))
         -1
       +1)))
  :config
  (progn
    ;; Rebind rinari keys.
    (define-prefix-command 'cb:rinari-map)
    (define-key rinari-minor-mode-map (kbd "C-c f") 'cb:rinari-map)
    (define-keys cb:rinari-map
      ";" 'rinari-find-by-context
      "C" 'rinari-find-cells
      "F" 'rinari-find-features
      "M" 'rinari-find-mailer
      "S" 'rinari-find-steps
      "Y" 'rinari-find-sass
      "a" 'rinari-find-application
      "c" 'rinari-find-controller
      "e" 'rinari-find-environment
      "f" 'rinari-find-file-in-project
      "h" 'rinari-find-helper
      "i" 'rinari-find-migration
      "j" 'rinari-find-javascript
      "l" 'rinari-find-lib
      "m" 'rinari-find-model
      "n" 'rinari-find-configuration
      "o" 'rinari-find-log
      "p" 'rinari-find-public
      "r" 'rinari-find-rspec
      "s" 'rinari-find-script
      "t" 'rinari-find-test
      "u" 'rinari-find-plugin
      "v" 'rinari-find-view
      "w" 'rinari-find-worker
      "x" 'rinari-find-fixture
      "y" 'rinari-find-stylesheet
      "z" 'rinari-find-rspec-fixture)

    (hook-fn 'rinari-minor-mode-hook
      (setq rinari-tags-file-name "TAGS"
            rng-nxml-auto-validate-flag nil
            nxml-degraded t))))

;; (use-package rsense
;;   :ensure t
;;   :defer  t
;;   :init
;;   (hook-fn 'cb:ruby-modes-hook
;;     (require 'rsense))
;;   :config
;;   (progn
;;     (define-path cb:rsense-home "bin/rsense-0.3")
;;     (setq rsense-home cb:rsense-home)
;;     (after 'auto-complete
;;       (add-to-list 'ac-sources 'ac-source-rsense))))

(use-package yari
  :ensure t
  :commands yari
  :init
  (hook-fn 'cb:ruby-modes-hook
    (local-set-key (kbd "C-c C-h") 'yari)))

(use-package inf-ruby
  :ensure   t
  :commands
  (inf-ruby-mode
   ruby-send-region)
  :config
  (defadvice ruby-switch-to-inf (around start-inf-ruby activate)
    "Start an inferior ruby if one is not running."
    (condition-case _
        ad-do-it
      (wrong-type-argument
       (run-ruby))))
  :init
  (after 'ruby-mode

    (defun cb-rb:inf-ruby-window ()
      (-when-let (buf (get-buffer inf-ruby-buffer))
        (--first-window (equal (window-buffer it) buf))))

    (defun restart-ruby ()
      (interactive)
      ;; Suppress exit query.
      (-when-let (proc (ignore-errors (inf-ruby-proc)))
        (set-process-query-on-exit-flag proc nil))
      ;; Kill and relaunch IRB, reusing existing window.
      (let ((win (cb-rb:inf-ruby-window)))
        (ignore-errors (kill-buffer inf-ruby-buffer))
        (save-window-excursion (run-ruby))
        (when win
          (set-window-buffer win inf-ruby-buffer))))

    (defun cb-rb:switch-to-ruby ()
      "Toggle between irb and the last ruby buffer.
Start an inferior ruby if necessary."
      (interactive)
      (cond
       ((derived-mode-p 'inf-ruby-mode)
        (switch-to-buffer-other-window
         (--first-buffer (derived-mode-p 'ruby-mode))))
       ((get-buffer inf-ruby-buffer)
        (ruby-switch-to-inf t))
       (t
        (run-ruby))))

    (defun cb-rb:eval-dwim ()
      "Perform a context-sensitive evaluation."
      (interactive)
      ;; Start ruby if necessary.
      (unless (get-buffer "*ruby*")
        (run-ruby)
        (cb-rb:switch-to-ruby)
        ;; Revert window layout.
        (when (= 2 (length (window-list)))
          (delete-other-windows)))
      (cond
       ;; Evaluate active region.
       ((region-active-p)
        (ruby-send-region (region-beginning) (region-end)))
       ;; Evaluate the block at or just before point.
       ((or (thing-at-point-looking-at
             (rx (or "end" "]" "}" ")") (* space) (* "\n")))
            (ruby-block-contains-point (point)))
        (ruby-send-block))
       ;; Eval the block-like thing around point.
       (t
        (ruby-send-region (line-beginning-position)
                          (line-end-position)))))

    (defun cb-rb:format-irb-error (lines)
      "Return a propertized error string for the given LINES of
an irb error message."
      (-when-let* ((err (--first (s-matches? "Error:" it) lines))
                   (colon (s-index-of ":" err)))
        (concat (propertize (substring err 0 colon) 'face 'error)
                (substring err colon))))

    (defun cb-rb:apply-font-lock (str)
      "Apply ruby font-locking to string STR."
      (with-temp-buffer
        (insert str)
        (require 'ruby-mode)
        ;; Configure ruby font-lock.
        (set (make-local-variable 'font-lock-defaults)
             '((ruby-font-lock-keywords) nil nil))
        (set (make-local-variable 'syntax-propertize-function)
             'ruby-syntax-propertize-function)

        (font-lock-fontify-buffer)
        (buffer-string)))

    (defun cb-rb:filter-irb-output (str &rest _)
      "Print IRB output to messages."
      (ignore-errors
        (when (and (fboundp 'inf-ruby-proc) (inf-ruby-proc))
          (let ((lines
                 (->> (s-lines str)
                   (--remove (or (s-contains? "--inf-ruby" it)
                                 (s-blank? it)
                                 (s-matches? inf-ruby-prompt-pattern it)))
                   (-map 's-trim))))
            (message (or (cb-rb:format-irb-error lines)
                         (cb-rb:apply-font-lock (car (reverse lines))))))))
      str)

    (inf-ruby-setup-keybindings)
    (define-keys ruby-mode-map
      "C-c C-c" 'cb-rb:eval-dwim
      "C-c C-z" 'cb-rb:switch-to-ruby)
    (define-key inf-ruby-mode-map (kbd "C-c C-z") 'cb-rb:switch-to-ruby)
    (define-key inf-ruby-minor-mode-map (kbd "C-c C-z") 'cb-rb:switch-to-ruby)

    (hook-fn 'inf-ruby-mode-hook
      (add-hook 'comint-preoutput-filter-functions 'cb-rb:filter-irb-output)
      ;; Stop IRB from echoing input.
      (setq comint-process-echoes t))))

(use-package ruby-tools
  :ensure   t
  :diminish ruby-tools-mode
  :commands ruby-tools-mode
  :init     (add-hook 'ruby-mode-hook 'ruby-tools-mode))

(use-package yaml-mode
  :ensure   t
  :commands yaml-mode
  :mode
  (("\\.yaml$" . yaml-mode)
   ("\\.yml$"  . yaml-mode)))

(use-package rvm
  :ensure t
  :commands
  (rvm-use-default
   rvm-activate-corresponding-ruby
   rvm-use
   rvm-open-gem)
  :init
  (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby))

(provide 'cb-ruby)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-ruby.el ends here
