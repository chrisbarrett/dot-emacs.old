;;; config-ruby.el --- Configure ruby

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

;; Configure ruby

;;; Code:

(require 'utils-common)
(require 'utils-buffers)
(require 'config-modegroups)
(require 'super-smart-ops)

(cb:declare-package-installer ruby
  :match (rx (or "Rakefile" "Vagrantfile" "Thorfile" "Capfile" "GuardFile" "Gemfile"
                 ".rb" ".ru" ".rake" ".jbuilder" ".thor" ".gemspec" ".podspec"))
  :packages (ruby-mode
             inf-ruby
             rvm
             rubocop))

(cb:declare-package-installer yaml
  :match (rx ".ya" (? "ml") eol)
  :packages (yaml-mode))

(add-to-list 'completion-ignored-extensions ".rbc")

(-each '(("\\.rake\\'". ruby-mode)
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
  (~ add-to-list 'auto-mode-alist))

(define-derived-mode erb-mode html-mode
  "ERB" nil
  (when (fboundp 'flycheck-mode)
    (flycheck-mode -1)))

(add-to-list 'auto-mode-alist '("\\.html\\.erb" . erb-mode))

(after 'ruby-mode

  (let ((file (f-join (-first (~ s-matches? "inf-ruby") (f-directories cb:elpa-dir))
                      "inf-ruby.el")))

    (autoload 'inf-ruby-mode file nil t)
    (load-file file))

  (after '(ruby-mode inf-ruby)

    (defadvice ruby-switch-to-inf (around start-inf-ruby activate)
      "Start an inferior ruby if one is not running."
      (condition-case _
          ad-do-it
        (wrong-type-argument
         (run-ruby))))

    (defun set-ruby-interpreter (cmd)
      "Set the default ruby interpreter to CMD."
      (interactive
       (list
        (ido-completing-read
         "Inferior Ruby Program: "
         (->> inf-ruby-implementations
           (-map 'car)
           (-filter 'executable-find)))))
      (setq inf-ruby-default-implementation cmd))

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
       ((and inf-ruby-buffer (get-buffer inf-ruby-buffer))
        (ruby-switch-to-inf t))
       (t
        (run-ruby))))

    (define-key ruby-mode-map (kbd "C-c C-z") 'cb-rb:switch-to-ruby)
    (define-key inf-ruby-mode-map (kbd "C-c C-z") 'cb-rb:switch-to-ruby)
    (define-key inf-ruby-minor-mode-map (kbd "C-c C-z") 'cb-rb:switch-to-ruby)

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

    (define-key ruby-mode-map (kbd "C-c C-c") 'cb-rb:eval-dwim)

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

    (hook-fn 'inf-ruby-mode-hook
      (add-hook 'comint-preoutput-filter-functions 'cb-rb:filter-irb-output)
      ;; Stop IRB from echoing input.
      (setq comint-process-echoes t))

    )

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
        (replace-match "\\1: " t nil))))

  (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)

  (add-hook 'cb:ruby-modes-hook 'subword-mode)

  (defun cb-rb:smart-colon ()
    "Insert a colon, with or without padding.
If this is the leading colon for a symbol, do not insert padding.
If this is the trailing colon for a hash key, insert padding."
    (interactive)
    (insert ":")
    (when (s-matches? (rx (+ alnum) ":" eol)
                      (buffer-substring (line-beginning-position) (point)))
      (just-one-space)))

  (--each cb:ruby-modes
    (super-smart-ops-configure-for-mode it
      :add '("~")
      :custom
      '(("," . (command (insert ",") (just-one-space)))
        (":" . cb-rb:smart-colon))))

  (require 'smartparens-ruby)

  (modify-syntax-entry ?@ "w" ruby-mode-syntax-table)
  (modify-syntax-entry ?_ "w" ruby-mode-syntax-table)
  (modify-syntax-entry ?! "w" ruby-mode-syntax-table)
  (modify-syntax-entry ?? "w" ruby-mode-syntax-table)

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
                   :pre-handlers '(sp-ruby-pre-handler))

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
                   :post-handlers '(sp-ruby-block-post-handler)))

  (hook-fn 'cb:ruby-modes-hook
    (local-set-key (kbd "C-c C-h") 'yari))

  (after 'hideshow
    (add-to-list 'hs-special-modes-alist
                 `(ruby-mode
                   ,(rx (or "def" "class" "module" "{" "[")) ; Block start
                   ,(rx (or "}" "]" "end"))                  ; Block end
                   ,(rx (or "#" "=begin"))                   ; Comment start
                   ruby-forward-sexp nil)))

  (after 'evil
    (define-evil-doc-handler cb:ruby-modes (call-interactively 'robe-doc)))

  )

(provide 'config-ruby)

;;; config-ruby.el ends here
