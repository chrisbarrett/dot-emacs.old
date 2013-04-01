;;; For testing...
(add-to-list 'load-path "~/dot-emacs-new/")
(setq user-emacs-directory (expand-file-name "~/dot-emacs-new/"))

;;; Disable intrusive GUI elements.
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(defun cb:byte-compile-lisp ()
  "Recompile all configuration files."
  (interactive)
  (byte-recompile-directory user-lisp-dir 0 t))

;; Initialize packages.
(require 'package)

(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(defun cb:require-package (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg))
  (require pkg))

(require 'cl-lib)
(require 'cb-load-path)
(require 'cb-macros)
(require 'cb-foundation)
(require 'cb-helm)
(require 'cb-ido)
(require 'cb-evil)
(require 'cb-colour)
(require 'cb-cosmetic)
(require 'cb-ediff)
(when (equal system-type 'darwin) (require 'cb-osx))
(require 'cb-shell)
(require 'cb-auto-complete)
(require 'cb-google)
(require 'cb-autopair)
(require 'cb-languages)
(require 'cb-indentation)
(require 'cb-json)
(require 'cb-lambda)
(require 'cb-markdown)
(require 'cb-compilation)
(require 'cb-text)
(require 'cb-flyspell)
(require 'cb-flycheck)
(require 'cb-yasnippet)
(require 'cb-makefile)
(require 'cb-shebang)
(require 'cb-xml)
(require 'cb-html)
(require 'cb-magit)

;; Local Variables:
;; no-byte-compile: t
;; lexical-binding: t
;; End:
