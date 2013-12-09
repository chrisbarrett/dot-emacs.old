;;; cb-paths.el --- Path variables common throughout config

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130924.0528

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

;; Path variables common throughout config. It is useful to keep these separate
;; for calls to the async library.

;;; Code:

(require 'cb-lib)

(defvar user-home-directory (concat (getenv "HOME") "/"))
(defvar user-dropbox-directory (concat user-home-directory "Dropbox/"))

(define-path cb:assets-dir       "assets/")
(define-path cb:autosaves-dir "tmp/autosaves/")
(define-path cb:backups-dir   "backups/")
(define-path cb:bin-dir       "bin/")
(define-path cb:el-get-dir    "el-get")
(define-path cb:elpa-dir      "elpa/")
(define-path cb:etc-dir       "etc/")
(define-path cb:lib-dir       "lib/" t)
(define-path cb:lisp-dir      "lisp/" t)
(define-path cb:src-dir       "src")
(define-path cb:tmp-dir       "tmp/")
(define-path cb:yasnippet-dir "snippets/")
(define-path cb:info-dir      "info")

(provide 'cb-paths)

;; Local Variables:
;; End:

;;; cb-paths.el ends here
