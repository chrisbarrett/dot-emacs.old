;;; config-sclang.el --- Configure sclang

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

;; Configure sclang

;;; Code:

(require 'utils-common)

(cb:declare-package-installer sclang
  :match (rx ".sc" eos)
  :packages '(sclang-extensions))

(autoload 'sclang-mode "sclang")
(autoload 'sclang-start "sclang")
(add-to-list 'auto-mode-alist '("\\.sc$" . sclang-mode))

(custom-set-variables
 '(sclang-auto-scroll-post-buffer t)
 '(sclang-eval-line-forward nil)
 '(sclang-show-workspace-on-startup nil))

(add-hook 'sclang-mode-hook 'sclang-extensions-mode)

;;; Commands

(defun supercollider ()
  "Start SuperCollider and open the SC Workspace."
  (interactive)
  (switch-to-buffer
   (get-buffer-create "*sclang workspace*"))
  (sclang-mode))

;;; Smart ops

(super-smart-ops-configure-for-mode 'sclang-mode :rem '("|"))

;;; Key bindings

(after 'sclang
  (define-key sclang-mode-map (kbd ".") nil))

(provide 'config-sclang)

;;; config-sclang.el ends here
