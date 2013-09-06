;;; cb-yasnippet.el --- Configuration for yasnippet

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130805.0332

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

;; Configuration for yasnippet

;;; Code:

(require 'use-package)
(require 'cb-lib)
(require 'cb-foundation)

(use-package yasnippet
  :ensure t
  :if (not noninteractive)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :diminish yas-minor-mode
  :commands
  (yas-global-mode
   yas-minor-mode
   snippet-mode)
  :init
  (progn
    (add-hook 'prog-mode-hook 'yas-minor-mode)
    (add-hook 'sgml-mode-hook 'yas-minor-mode))
  :config
  (progn
    (setq
     yas-prompt-functions'(yas-ido-prompt)
     yas/trigger-key (kbd "RET"))
    (add-to-list 'yas-snippet-dirs cb:yasnippet-dir)
    (yas-global-mode t)
    (hook-fn 'snippet-mode-hook
      (setq require-final-newline nil))))

(provide 'cb-yasnippet)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-yasnippet.el ends here
