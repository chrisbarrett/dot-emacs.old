;;; cb-windowing.el --- Configuration for window management

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0020

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

;; Configuration for window management

;;; Code:

(require 'use-package)
(require 'cb-foundation)
(require 'cb-evil)

;; Bury the compilation log once Emacs has started.
(hook-fn 'after-init-hook
  (let ((bufname "*Compile-Log*"))
    (when (get-buffer bufname)
      (bury-buffer bufname))))

(use-package workgroups
  :ensure t
  :defer  t
  :idle   (require 'workgroups)
  :bind
  (("s-1" . wg-switch-to-index-0)
   ("s-2" . wg-switch-to-index-1)
   ("s-3" . wg-switch-to-index-2)
   ("s-4" . wg-switch-to-index-3)
   ("s-5" . wg-switch-to-index-4))
  :diminish workgroups-mode
  :commands workgroups-mode
  :config
  (progn
    (setq
     wg-display-current-workgroup-left-decor "["
     wg-display-current-workgroup-right-decor "]"
     wg-morph-vsteps 6
     wg-prefix-key (kbd "C-c w"))
    (wg-set-prefix-key)

    (defadvice wg-mode-line-add-display (around wg-suppress-error activate)
      "Ignore errors in modeline display function caused by custom modeline."
      (ignore-errors ad-do-it))))

(use-package transpose-frame
  :bind
  (("C-x t" . transpose-frame)
   ("s-t"   . transpose-frame)
   ("C-x f" . rotate-frame)
   ("s-r"   . rotate-frame))

  :commands
  (transpose-frame
   flip-frame
   flop-frame
   rotate-frame
   rotate-frame-clockwise
   rotate-frame-anticlockwise))

(use-package winner
  :commands winner-mode
  :init (hook-fn 'window-configuration-change-hook
          (unless (true? winner-mode)
            (winner-mode +1))))

(use-package window-number
  :ensure t
  :defer  t
  :commands window-number-meta-mode
  :init (hook-fn 'window-configuration-change-hook (window-number-meta-mode +1)))

(provide 'cb-window-management)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; End:

;;; cb-window-management.el ends here
