;;; config-popwin.el --- Configuration for popwin

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

;; Configuration for popwin

;;; Code:

(require 'utils-common)

(cb:install-package 'popwin t)

(custom-set-variables
 '(display-buffer-function 'popwin:display-buffer)
 '(popwin:special-display-config
   '(("*Help*" :height 30 :stick t)
     ("*Completions*" :noselect t)
     ("*Shell Command Output*")
     ("*compilation*" :noselect t)
     ("*Compile-Log*" :height 7 :noselect t)
     ("*Messages*" :height 30)
     ("* Racket REPL *" :height 7 :stick t)
     ("*Geiser dbg*" :height 7)
     ("*execute scheme*" :height 7 :noselect t)
     ("*haskell*" :height 7 :stick t)
     ("*Directory*")
     ("*Org Note*")
     ("*response*" :height 7 :noselect t)
     ("*rope-pydoc*" :position bottom)
     ("*robe-doc*" :height 30)
     ("*idris-repl*" :height 30)
     ("*idris-notes*")
     ("*idris-info*")
     ("*utop*" :height 30)
     ("\\*cider-repl " :height 7 :regexp t :stick t)
     ("*cider doc*" :height 30)
     ("*bbdb*")
     ("*BBDB*")
     ("*messages*")
     ("*Occur*" :noselect t)
     ("\\*Slime Description.*" :noselect t :regexp t :height 30)
     ("\\*Slime Inspector.*" :regexp t :height 30)
     ("*sldb.*" :regexp t :height 30)
     ("*Ido Completions*" :noselect t :height 30)
     (".*overtone.log" :regexp t :height 30)
     ("*gists*" :height 30))))


(provide 'config-popwin)

;;; config-popwin.el ends here
