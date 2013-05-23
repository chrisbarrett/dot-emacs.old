;;; cb-haskell --- Haskell editing commands and advice.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Haskell editing commands and advice.

;;; Code:

;;; Testing

(autoload 'haskell-cabal-find-file "haskell-cabal")
(require 's)

(defun cb:hs-project-root ()
  (let ((cabal (haskell-cabal-find-file)))
    (when cabal (file-name-directory cabal))))

(defun cb:hs-srcfile->testfile (fname)
  (s-replace "/src/" "/test/"
             (replace-regexp-in-string "\\(.\\)l?hs$" "Tests." fname t nil 1)))

(defun cb:hs-testfile->srcfile (fname)
  (s-replace "/test/" "/src/"
             (replace-regexp-in-string "\\(Tests\\).l?hs$" "" fname t nil 1)))

(defun cb:hs-test<->code ()
  (interactive)
  (let* ((src-p (s-contains? "/src/" (buffer-file-name)))
         (file (if src-p
                   (cb:hs-srcfile->testfile (buffer-file-name))
                 (cb:hs-testfile->srcfile (buffer-file-name)))))
    (when (cb:hs-project-root)
      (cond
       ((file-exists-p file) (find-file file))
       (src-p (error "No corresponding unit test file found"))
       (t     (error "No corresponding source file found"))))))

;;; Use greek lambda symbol.
(font-lock-add-keywords 'haskell-mode
 `((,(concat "\\s (?\\(\\\\\\)\\s *\\(\\w\\|_\\|(.*)\\).*?\\s *->")
    (0 (progn (compose-region (match-beginning 1) (match-end 1)
                              ,(make-char 'greek-iso8859-7 107))
              nil)))))

(provide 'cb-haskell)

;;; cb-haskell.el ends here
