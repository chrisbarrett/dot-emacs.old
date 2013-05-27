;;; cb-supercollider.el --- Configuration for SuperCollider

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130526.2359

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

;; Configuration for SuperCollider

;;; Code:

(require 'use-package)

(use-package sclang
  :commands
  (sclang-mode
   sclang-start)
  :mode ("\\.sc$" . sclang-mode)
  :init
  (defun supercollider ()
    "Start SuperCollider and open the SC Workspace."
    (interactive)
    (switch-to-buffer
     (get-buffer-create "*sclang workspace*"))
    (sclang-mode))

  :config
  (progn
    (setq sclang-auto-scroll-post-buffer   t
          sclang-eval-line-forward         nil
          sclang-show-workspace-on-startup nil)
    (hook-fn 'sclang-mode-hook
      (smartparens-mode +1))))

(use-package sclang-snippets
  :ensure t
  :defer t
  :init
  (hook-fn 'sclang-mode
    (sclang-snippets-initialize)
    (add-to-list 'ac-sources 'ac-source-yasnippet)))

(use-package sclang-extensions
  :ensure   t
  :commands sclang-extensions-mode
  :init     (add-hook 'sclang-mode-hook 'sclang-extensions-mode))

(provide 'cb-supercollider)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-supercollider.el ends here
