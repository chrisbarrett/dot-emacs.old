;;; cb-auto-complete.el --- Configuration for auto-complete

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130805.0331

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

;; Configuration for auto-complete

;;; Code:

(require 'use-package)
(require 'cb-lib)
(require 'cb-paths)

(use-package auto-complete
  :ensure   t
  :defer t
  :commands 'global-auto-complete-mode
  :diminish auto-complete-mode

  :init
  (hook-fn 'after-init-hook
    (global-auto-complete-mode +1))

  :config
  (progn

    (require 'auto-complete-config)
    (ac-config-default)

    (defadvice ac-quick-help (around ignore-errors activate)
      "Ignore errors when showing help popups."
      (ignore-errors ad-do-it))

    (add-to-list 'ac-dictionary-directories
                 (concat user-emacs-directory "ac-dict"))

    (setq
     ac-auto-show-menu t
     ac-dwim t
     ac-use-menu-map t
     ac-quick-help-delay 0.4
     ac-quick-help-height 60
     ac-disable-inline t
     ac-show-menu-immediately-on-auto-complete t
     ac-auto-start 2
     ac-candidate-menu-min 0
     ac-comphist-file (concat cb:tmp-dir "ac-comphist.dat"))

    (ac-flyspell-workaround)

    (define-keys ac-completing-map
      "C-n"   'ac-next
      "C-p"   'ac-previous
      "\t"    'ac-complete
      "M-RET" 'ac-help)))

(provide 'cb-auto-complete)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-auto-complete.el ends here
