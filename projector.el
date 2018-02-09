;;; projector.el --- An Emacs interface for the projector tool.

;; Copyright (C) 2018 Mathew Robinson <chasinglogic@gmail.com>

;; Author: Mathew Robinson <chasinglogic@gmail.com>
;; Version: 1.0
;; Keywords: cli, dotfiles, projector
;; URL: https://github.com/chasinglogic/projector.el

;; This program is free software; you can redistribute it and/or modify
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

;; projector.el is a wrapper script around the projector (DotFile Manager) tool that can be
;; found here: https://github.com/chasinglogic/projector. It provides commands for
;; running projector as well as adding hooks to automatically sync your dotfiles on
;; save.

;; Full documentation is available as an Info manual.

;;; Code:

(require 's)
(require 'dash)

(defcustom projector-code-dir (getenv "CODE_DIR")
  "Where projector should search for projects")

(defun projector--command (command &rest command-args)
  (let* ((name (format "projector %s" command))
        (buffer-name (format "*%s output*" name))
        (process-args
         (append (list "projector")
                 (if command-args
                     (append (list command) command-args)
                   (list command))
                 ))
         )
    (make-process
     :name name
     :buffer buffer-name
     :command process-args)))

(defun projector--get-projects ()
  "Return a list of projects projector knows about"
  (-filter (lambda (e) (not (string= "" e)))
           (s-split "\n"
                    (shell-command-to-string
                     (format "projector --code-dir %s list" projector-code-dir)))))

(defun projector-add-projects-to-projectile-known-projects ()
  "Add all projects projector knows about to projectile-known-projects"
  (interactive)
  (let* ((projector-projects (projector--get-projects))
         (new-project-list (append projectile-known-projects projector-projects))
         (no-duplicates (remove-duplicates new-project-list :test 'string=)))
    (setq projectile-known-projects no-duplicates)))

(defun projector-command (shell-command)
  "Run shell command in all projector projects"
  (interactive "sShell Command: ")
  (shell-command
   (format "projector --code-dir %s run %s" projector-code-dir shell-command)
   "*projector run output*"))
(defalias 'projector-run 'projector-command)

(provide 'projector)
;;; projector.el ends here
