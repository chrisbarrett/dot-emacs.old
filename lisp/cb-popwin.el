;;; cb-popwin.el --- Configuration for popwin

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130927.1156

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

;; Configuration for popwin.

;;; Code:

;; `popwin' allows you to override the default handling of popup buffers.
(use-package popwin
  :if (not noninteractive)
  :ensure t
  :config
  (progn
    (hook-fn 'window-configuration-change-hook
      (unless (true? popwin-mode)
        (popwin-mode +1)))

    (setq display-buffer-function 'popwin:display-buffer
          popwin:special-display-config
          '(("*Help*"  :height 30 :stick t)
            ("*Completions*" :noselect t)
            ("*Shell Command Output*")
            ("*compilation*" :noselect t)
            ("*Compile-Log*" :height 20 :noselect t)
            ("*Messages*" :height 30)
            ("*Directory*")
            ("*Org Note*")
            ("*jedi:doc*" :height 30)
            ("*bbdb*")
            ("*BBDB*")
            ("*execute scheme*" :height 15)
            ("*Occur*" :noselect t)
            ("\\*Slime Description.*" :noselect t :regexp t :height 30)
            ;; ("*magit-commit*" :noselect t :height 40 :width 80)
            ;; ("*magit-diff*" :height 40 :width 80)
            ;; ("*magit-edit-log*" :noselect t :height 15 :width 80)
            ("\\*Slime Inspector.*" :regexp t :height 30)
            ("*Ido Completions*" :noselect t :height 30)
            ;; ("*eshell*" :height 30)
            ;; ("*shell*" :height 30)
            (".*overtone.log" :regexp t :height 30)
            ("*gists*" :height 30)
            ("*sldb.*" :regexp t :height 30)
            ("*robe-doc*" :height 30)))))

(provide 'cb-popwin)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-popwin.el ends here