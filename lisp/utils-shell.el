;;; utils-shell.el --- Utilities for managing buffers and windows

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

;; Utilities for managing buffers and windows

;;; Code:

(require 'utils-common)

(defvar %-sudo-liftable-commands '(%-sh
                                   %-async
                                   %-string
                                   shell-command
                                   async-shell-command
                                   shell-command-to-string)
  "A list of commands that may be escalated using the `%-sudo' macro.

`%-sudo' operates by modifying the string passed to the shell.
For this to work, all commands in this list must accept a string
as their first parameter.")

(defalias '%-quote 'shell-quote-argument)

(defun %-sh (command &rest arguments)
  "Run COMMAND with ARGUMENTS, returning the exit code."
  (shell-command (concat command " " (s-join " " arguments))))

(defun %-string (command &rest arguments)
  "Run COMMAND with ARGUMENTS, returning its output as a string."
  (s-trim-right
   (shell-command-to-string (concat command " " (s-join " " arguments)))))

(defun %-async (command &rest arguments)
  "Run COMMAND with ARGUMENTS asynchronously."
  (save-window-excursion
    (async-shell-command (concat command " " (s-join " " arguments)))))

(defun %-can-sudo-without-passwd? ()
  "Test whether we are currently able to sudo without entering a password."
  (zerop (shell-command "sudo -n true")))

(defmacro %-sudo (command)
  "Execute a shell command with escalated privileges.

COMMAND must be a direct call to one of the forms listed in
`sudo-liftable-commands'.

The sudo command will likely be configured with a timeout on your
system.  The user will be interactively prompted for their
password if necessary.  Subsequent calls to sudo within the
timeout period will not require the password again."
  (cl-assert command)
  (cl-assert (listp command))
  (cl-assert (-contains? %-sudo-liftable-commands (car command)))

  ;; Reach into the command and replace the direct shell command argument,
  ;; wrapping it with a call to sudo.
  ;;
  ;; There are two execution paths, depending on whether the user is currently
  ;; authenticated with sudo.
  (cl-destructuring-bind (fn cmd &rest args) command
    (let ((g-passwd (cl-gensym))
          (g-result (cl-gensym)))
      `(-if-let (,g-passwd (unless (%-can-sudo-without-passwd?)
                             (read-passwd "Password: ")))

           ;; Path 1. The password is required: Consume the password and
           ;; tidy the shell output. Finally, delete the password string from
           ;; memory.
           (unwind-protect
               (let ((,g-result
                      (,fn
                       (format "echo %s | sudo -S %s"
                               (shell-quote-argument ,g-passwd) ,cmd)
                       ,@args)))
                 ;; Annoyingly, the password prompt gets prepended to string
                 ;; output and must be stripped.
                 (if (stringp ,g-result)
                     (s-chop-prefix "Password:" ,g-result)
                   ,g-result))
             ;; Clear the password from memory.
             (clear-string ,g-passwd))

         ;; Path 2. We are within the sudo timeout period: The password is not
         ;; required and we can call the command with sudo prefixed.
         (,fn (format "sudo %s" ,cmd) ,@args)))))

(provide 'utils-shell)

;;; utils-shell.el ends here
