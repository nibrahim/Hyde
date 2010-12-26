;;; hyde-git.el
;; Copyright (C) 2004 Noufal Ibrahim <noufal at nibrahim.net.in>
;;
;; This program is not part of Gnu Emacs
;;
;; hyde-git.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

(defun hyde/git/uncommittedp (repo file)
  "Returns true if there are uncommitted changes for the current file"
  (let (
	(cmd (format "cd '%s' && git diff-files --quiet '%s' > /dev/null" repo file))
	)
    (message (concat " **** uncommittedp " cmd))
    (= (shell-command cmd) 1)))

(defun hyde/git/unpushedp (repo file)
  "Returns true if there are unpushed changes for the current file"
  (let (
	(cmd (format "cd '%s' && git log --exit-code origin/master..HEAD '%s' > /dev/null" repo file))
	)
    (message (concat " **** unpushedp " cmd))
    (= (shell-command cmd) 1)))

(defun hyde/git/pushedp (repo file)
  "Returns true if there are no uncommitted changes in the file"
  (not (hyde/git/uncommittedp repo file)))

(defun hyde/git/add (repo file)
  "Adds the given file to the repository"
  (let ((cmd (format "cd '%s' && git add '%s' > /dev/null" repo file)))
    (shell-command cmd)))

(defun hyde/git/commit (repo file commit-message)
  "Commits the given file to the repository"
  (let ((cmd (format "cd '%s' && git commit -m '%s' '%s' > /dev/null" repo commit-message file)))
    (shell-command cmd)))

(defun hyde/git/push (repo)
  "Pushes the repository"
  (let ((cmd (format "cd '%s' && git push origin master > /dev/null" repo)))
    (message (concat " **** Pushing " cmd))
    (shell-command cmd)))

(defun hyde/git/rename (base from to)
  "Rename the file in BASE from FROM to TO"
  (let ((cmd (format "cd '%s' && git mv '%s' '%s' > /dev/null" base from to)))
    (message (concat " **** Renaming " cmd))
    (shell-command cmd)))


(provide 'hyde-git)
