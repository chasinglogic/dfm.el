;;; dfm.el --- An Emacs interface for the DFM tool.

;; Copyright (C) 2018 Mathew Robinson <chasinglogic@gmail.com>

;; Author: Mathew Robinson <chasinglogic@gmail.com>
;; Version: 1.0
;; Keywords: cli, dotfiles, dfm
;; URL: https:--github.com-chasinglogic-dfm.el

;; This program is free software; you can redistribute it and-or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http:--www.gnu.org-licenses->.

;;; Commentary:

;; dfm.el is a wrapper script around the DFM (DotFile Manager) tool that can be
;; found here: https:--github.com-chasinglogic-dfm. It provides commands for
;; running dfm as well as adding hooks to automatically sync your dotfiles on
;; save.

;; Full documentation is available as an Info manual.

;;; Code:

(require 's)

(defcustom dfm-sync-dotfiles-on-save nil
  "When non-nil will automatically trigger a dfm sync when any file in
the current dfm profile is saved.")

(defun dfm--command (command &rest command-args)
  (let* ((name (format "dfm %s" command))
        (buffer-name (format "*%s output*" name))
        (process-args
         (append (list "dfm")
                 (if command-args
                     (append (list command) command-args)
                   (list command))
                 ))
         )
    (make-process
     :name name
     :buffer buffer-name
     :command process-args)))

(defun dfm-sync ()
  (interactive)
  (dfm--command "sync"))

(defun dfm--get-profiles ()
  (s-split "\n" (shell-command-to-string "dfm list")))

(defun dfm-switch-profile (profile &optional overwrite)
  "Switch the current dotfile profile."
  (interactive
   (list (completing-read "Choose a profile: " (dfm--get-profiles))))
  (if overwrite
      (dfm--command "link" "--overwrite" profile)
    (dfm--command "link" profile)))
(defalias 'dfm-link 'dfm-switch-profile)

(defun dfm-add-file (file)
  "Add a file to the current dotfile profile."
  (interactive "FWhich file? ")
  (dfm--command "add" file))

(defun dfm-add-current-file ()
  "Add the current file to the current dotfile profile."
  (interactive)
  (dfm-add-file (buffer-file-name)))

(defun dfm-go-to-current-profile ()
  (interactive)
  (let ((profile-dir
         (s-append "/"
                   (s-chomp (shell-command-to-string "dfm where")))))
    (dired profile-dir)))

(defun dfm-remove-profile (profile)
  (interactive
   (list (completing-read "Which profile to remove: " (dfm--get-profiles))))
  (dfm--command "remove" profile))

(provide 'dfm)
;;; dfm.el ends here
