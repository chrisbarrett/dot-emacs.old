;;; For testing...
(add-to-list 'load-path "~/dot-emacs-new/")
(setq user-emacs-directory (expand-file-name "~/dot-emacs-new/"))

;;; Disable intrusive GUI elements.
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

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

(cb:require-package 'dash)
(cb:require-package 's)
(require 'cl-lib)
(require 'cb-load-path)
(require 'cb-macros)
(require 'cb-foundation)
(require 'cb-ido)

;; Local Variables:
;; no-byte-compile: t
;; End:
