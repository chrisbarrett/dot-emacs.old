;;; config-discover.el --- Configuration for discover.el  -*- lexical-binding: t; -*-

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

;; Configuration for discover.el

;;; Code:

(require 'utils-common)

(cb:install-package 'discover t)
(global-discover-mode +1)

(defmacro search:fn-with-thing-at-point (binding &rest body)
  "Bind BINDING to the symbol at point around BODY forms."
  (declare (indent 1))
  `(lambda ()
     (interactive)
     (let ((,binding
            (let ((s (or (current-region) (thing-at-point 'symbol))))
              (when s
                (substring-no-properties s)))))
       ,@body)))

(defun search:read-string (source-name &optional default)
  "Read a query for SOURCE-NAME with an optional DEFAULT."
  (let ((prompt (if default
                    (format "%s (default: %s): " source-name default)
                  (format "%s: " source-name))))
    (read-string prompt nil t default)))

;;; Search menu

(discover-add-context-menu
 :bind "C-c s"
 :context-menu
 `(cb-search
   (description "Search commands")
   (actions

    ("Dictionary"
     ("d" "Dictionary"
      ,(search:fn-with-thing-at-point q
         (dictionary-search (search:read-string "Dictionary" q)))))

    ("Org"
     ("o" "Org files" org-search-view))

    ("Internet"
     ("g" "Google Web"
      ,(search:fn-with-thing-at-point q
         (browse-url
          (concat "https://www.google.com/search?q="
                  (url-hexify-string (search:read-string "Google Web" q))))))

     ("i" "Google Images"
      ,(search:fn-with-thing-at-point q
         (browse-url
          (concat "https://www.google.co.nz/search?tbm=isch&q="
                  (url-hexify-string (search:read-string "Google Images" q))))))

     ("y" "YouTube"
      ,(search:fn-with-thing-at-point q
         (browse-url
          (concat "http://www.youtube.com/results?search_query="
                  (url-hexify-string (search:read-string "YouTube" q))))))

     ("w" "Wikipedia"
      ,(search:fn-with-thing-at-point q
         (browse-url
          (concat "http://en.wikipedia.org/w/index.php?search="
                  (url-hexify-string (search:read-string "Wikipedia" q)))))))

    ("System"
     ("i" "info" helm-info-at-point)
     ("m" "manpages"
      ,(search:fn-with-thing-at-point q
         (require 'helm-man)
         (helm :sources 'helm-source-man-pages
               :buffer "*Helm man woman*"
               :input q)))))))

;;; Help menu

(discover-add-context-menu
 :context-menu
 `(cb-help-locate
   (description "Goto definitions")
   (actions
    ("Find"
     ("f" "Function" find-function)
     ("F" "Face" find-face)
     ("l" "Library" find-library)
     ("v" "Variable" find-variable)))))

(discover-add-context-menu
 :context-menu
 '(cb-help-emacs
   (description "Emacs information")
   (actions
    ("About"
     ("a" "About Emacs"        about-emacs)
     ("c" "Copying"            describe-copying)
     ("f" "FAQ"                view-emacs-FAQ)
     ("g" "GNU project"        describe-gnu-project))
    ("Documentation"
     ("d" "Debugging"          view-emacs-debugging)
     ("e" "External packages"  view-external-packages)
     ("m" "Emacs manual"       info-emacs-manual)
     ("n" "News"               view-emacs-news)))))

(discover-add-context-menu
 :context-menu
 '(cb-help-describe
   (description "Describe Emacs features")
   (actions
    ("Describe"
     ("b" "Key bindings"     describe-bindings)
     ("c" "Coding system"    describe-coding-system)
     ("i" "Input method"     describe-input-method)
     ("k" "Key"              describe-key)
     ("l" "Language"         describe-language-environment)
     ("m" "Mode"             describe-mode)
     ("s" "Syntax"           describe-syntax)
     ("t" "Theme"            describe-theme))
    ("Lisp"
     ("f" "Function"         describe-function)
     ("v" "Variable"         describe-variable)
     ("F" "Face"             describe-face)
     ("p" "Package"          describe-package)))))

(discover-add-context-menu
 :context-menu
 '(cb-help-info
   (description "Info")
   (actions
    ("Info"
     ("i" "Info contents"  info)
     ("f" "Find command"   Info-goto-emacs-command-node)
     ("k" "Find key"       Info-goto-emacs-key-command-node)
     ("s" "Lookup symbol"  info-lookup-symbol)))))

(discover-add-context-menu
 :bind "C-h"
 :context-menu
 '(cb-help
   (description "Emacs help commands")
   (actions
    ("Emacs"
     ("C-h" "Emacs manual"   info-emacs-manual)
     ("t"   "Emacs Tutorial" help-with-tutorial)
     ("?"    "Emacs info..." makey-key-mode-popup-cb-help-emacs))
    ("Describe"
     ("F" "Face"             describe-face)
     ("f" "Function"         describe-function)
     ("k" "Key"              describe-key)
     ("p" "Package"          describe-package)
     ("v" "Variable"         describe-variable))
    ("Lisp"
     ("e" "Locate..."        makey-key-mode-popup-cb-help-locate))
    ("Documentation"
     ("C-i" "Info contents"  info)
     ("i" "Info..."          makey-key-mode-popup-cb-help-info)
     ("d" "Describe..."      makey-key-mode-popup-cb-help-describe))
    ("Apropos"
     ("a" "Command"          apropos-command)
     ("D" "Documentation"    apropos-documentation))
    ("Environment"
     ("m" "Messages"         view-echo-area-messages)
     ("l" "Lossage"          view-lossage)))))

;;; Sorting

(discover-add-context-menu
 :context-menu
 '(cb-sort
   (description "Sorting commands")
   (actions
    ("Sort"
     ("a" "Alpha" sort-lines)
     ("A" "Alpha (reverse)" (lambda () (sort-lines t (region-beginning) (region-end))))
     ("r" "Reverse" reverse-region)))))

(defun cb:sort-dispatch ()
  "Open the appropriate sorting picker for the current mode."
  (interactive)
  (cond
   ((derived-mode-p 'org-mode)
    (call-interactively 'org-sort))
   ((region-active-p)
    (call-interactively 'makey-key-mode-popup-cb-sort))
   (t
    (user-error "Sort commands require a region to be active"))))

(bind-key* "C-c ^" 'cb:sort-dispatch)

;;; Yasnippet

(discover-add-context-menu
 :bind "C-c y"
 :context-menu
 '(cb-yasnippet
   (describe "Yasnippet commands")
   (actions
    ("Create and edit"
     ("e" "Expand" yas-expand)
     ("f" "Visit File" yas-visit-snippet-file)
     ("i" "Insert" yas-insert-snippet)
     ("n" "New" yas-new-snippet))
    ("Snippet management"
     ("r" "Reload All" cbyas:reload-all)
     ("q" "Exit Snippets" yas-exit-all-snippets)
     ("t" "Show Tables" yas-describe-tables)))))

;;; Projectile

(discover-add-context-menu
 :context-menu
 '(cb-projectile-other-window
   (describe "Project management and navigation (other window)")
   (actions
    ("File"
     ("f" "Find file"       projectile-find-file-other-window))
    ("Directory"
     ("d" "Find dir"        projectile-find-dir-other-window))
    ("Test"
     ("t" "Switch to/from"  projectile-find-implementation-or-test-other-window))
    ("Buffer"
     ("b" "Switch"          projectile-switch-to-buffer-other-window)
     ("s" "Display"         projectile-display-buffer)))))

(discover-add-context-menu
 :bind "C-c p"
 :context-menu
 '(cb-projectile
   (describe "Project management and navigation")
   (actions
    ("Shell"
     ("!" "Shell command"                  projectile-run-shell-command-in-root)
     ("&" "Shell command (async)"          projectile-run-async-shell-command-in-root))

    ("Search & Replace"
     ("a" "Ack"                            projectile-ack)
     ("c" "Occur"                          projectile-multi-occur)
     ("r" "Replace"                        projectile-replace))

    ("File"
     ("f" "Find file"                      projectile-find-file)
     ("F" "Find in projects"               projectile-find-file-in-known-projects)
     ("e" "Recent"                         projectile-recentf)
     ("l" "Find in dir"                    projectile-find-file-in-directory))

    ("Directory"
     ("d" "Find dir"                       projectile-find-dir)
     ("D" "Dired"                          projectile-dired))

    ("Tags"
     ("j" "Find"                           projectile-find-tag)
     ("R" "Rebuild"                        projectile-regenerate-tags))

    ("Test"
     ("t" "Switch to/from"                 projectile-toggle-between-implementation-and-test)
     ("T" "Find test"                      projectile-find-test-file)
     ("p" "Run tests"                      projectile-test-project))

    ("Project"
     ("s" "Switch"                         projectile-switch-project)
     ("S" "Save"                           projectile-save-project-buffers)
     ("v" "VC"                             projectile-vc))

    ("Buffer"
     ("b" "Switch"                         projectile-switch-to-buffer)
     ("i" "IBuffer"                        projectile-ibuffer)
     ("k" "Kill buffers"                   projectile-kill-buffers)
     ("x" "Most recent"                    projectile-project-buffers-other-buffer))

    ("Cache"
     ("I" "Invalidate"                     projectile-invalidate-cache)
     ("z" "File"                           projectile-cache-current-file))

    ("Other"
     ("o" "Other window..."                makey-key-mode-popup-cb-projectile-other-window)
     ("<backspace>" "Delete a project"     projectile-delete-project)))))

;;; Org

(require 'config-orgmode)

(discover-add-context-menu
 :bind "<f7>"
 :context-menu
 `(cb-org-capture
   (description "Org capture commands")
   (actions
    ("Capture"
     ,@(--map (cl-destructuring-bind (key desc &rest _) it
                (list key desc (command (org-capture nil key))))
              org-capture-templates)))))

(discover-add-context-menu
 :bind "C-c o"
 :context-menu
 `(cb-org
   (description "Orgmode commands")
   (actions

    ("Navigation"
     ("$" "Go to ledger"      ,(command (find-file ledger-file)))
     ("a" "Agenda"            org-agenda)
     ("b" "Buffers"           org-iswitchb)
     ("c" "Follow Clock"      org-clock-goto)
     ("d" "Go to Diary"       cb-org:find-diary)
     ("g" "Go to Subtree"     ,(command (org-refile 'goto)))
     ("n" "Go to Notes"       cb-org:find-notes)
     ("s" "Search"            org-search-view)
     ("w" "Go to Work"       ,(command (find-file org-work-file))))

    ("Capture"
     ("k" "Capture"           makey-key-mode-popup-cb-org-capture)
     ("l" "Store Link"        org-store-link))

    ("Todo & Tag"
     ("t" "Todo List"         executor:org-show-todo-list)
     ("v" "View Tags (todos)" executor:org-tags-view-todos-fullscreen)
     ("V" "View Tags (all)"   executor:org-tags-view-all-fullscreen))

    ("Drill"
     ("r" "Org Drill" ,(command (org-drill 'agenda)))))))

;;; Scanning

(discover-add-context-menu
 :bind "<f10>"
 :context-menu
 '(cb-scan
   (description "Printer and scanner commands")
   (actions
    ("Printing"
     ("p" "Print Buffer" print-buffer)
     ("r" "Print Region" print-region))

    ("Scanning"
     ("s" "Scan (flatbed)" scan)
     ("m" "Scan Multiple & Combine (flatbed)" scan-then-combine)
     ("u" "Scan (document feeder)" scan-batch-to-file)
     ("a" "Scan and Attach (org)" org-attach-from-scanner)))))

(provide 'config-discover)

;;; config-discover.el ends here
