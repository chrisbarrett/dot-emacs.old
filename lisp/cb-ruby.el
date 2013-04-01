;;; cb-ruby.el

(cb:require-package 'rsense)
(cb:require-package 'inf-ruby)
(cb:require-package 'ruby-electric)
(cb:diminish 'ruby-electric-mode)

(cb:auto-mode-on-match 'ruby-mode "\\.rake$")
(cb:auto-mode-on-match 'ruby-mode "Rakefile$")
(cb:auto-mode-on-match 'ruby-mode "\\.gemspec$")

(add-to-list 'completion-ignored-extensions ".rbc")

(cb:define-path cb:rsense-home "bin/rsense-0.3")
(setq rsense-home cb:rsense-home)

(add-to-list 'ac-modes 'ruby-mode)
(setq ruby-electric-expand-delimiters-list '(39 96 124))

(defun cb:on-ruby-mode ()
  (add-to-list 'ac-sources 'ac-source-rsense-method)
  (add-to-list 'ac-sources 'ac-source-rsense-constant)
  (ruby-electric-mode t)
  (inf-ruby-setup-keybindings))

(add-hook 'ruby-mode-hook 'cb:on-ruby-mode)

(provide 'cb-ruby)
