;;; init.el --- Bootstrap configuration

;; Copyright (C) 2013, 2014 Chris Barrett

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

;;; Code:

(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'utils-buffers)
(require 'utils-commands)
(require 'utils-common)
(require 'utils-shell)
(require 'utils-ui)
(require 'config-modegroups)
(require 'config-base)
(require 'config-bbdb)
(require 'config-calc)
(require 'config-company)
(require 'config-compilation)
(require 'config-darwin)
(require 'config-diff)
(require 'config-dired)
(require 'config-eshell)
(require 'config-evil)
(require 'config-expand-region)
(require 'config-file-template)
(require 'config-flycheck)
(require 'config-fortune)
(require 'config-git)
(require 'config-helm)
(require 'config-highlight-symbol)
(require 'config-hl-line)
(require 'config-ido)
(require 'config-iedit)
(require 'config-info)
(require 'config-insertion)
(require 'config-irc)
(require 'config-ledger)
(require 'config-mail)
(require 'config-modeline)
(require 'config-pickers)
(require 'config-popwin)
(require 'config-projectile)
(require 'config-recentf)
(require 'config-scanner)
(require 'config-search)
(require 'config-server)
(require 'config-skeletor)
(require 'config-smartparens)
(require 'config-spelling)
(require 'config-theme)
(require 'config-undo-tree)
(require 'config-w3m)
(require 'config-whitespace)
(require 'config-yasnippet)
(require 'config-languages)
(require 'config-orgmode)
(require 'custom)
(require 'personal-config nil t)

(provide 'init)

;;; init.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
