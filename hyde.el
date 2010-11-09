;;; hyde.el
;; Copyright (C) 2004 Noufal Ibrahim <noufal at nibrahim.net.in>
;;
;; This program is not part of Gnu Emacs
;;
;; hyde.el is free software; you can redistribute it and/or modify it
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


;; (defcustom hyde-home
;;   "/home/noufal/blog"
;;   "Root directory where your entire blog resides")

(setq hyde-home "/home/noufal/blog")

;; (defcustom hyde-deploy-dir
;;   "_site"
;;   "Directory which needs to be deployed")

(setq hyde-deploy-dir "_site")

;; (defcustom hyde-posts-dir 
;;   "_posts"
;;   "Directory which contains the list of posts")

(setq hyde-posts-dir "_posts")

;; (defcustom hyde/hyde-list-posts-command 
;;   "/bin/ls -1tr /tmp"
;;   "Command to list the posts")

(setq hyde/hyde-list-posts-command "/bin/ls -1tr")


(defun hyde/load-posts ()
  "Load up the posts and present them to the user"
  (interactive)
  (switch-to-buffer (get-buffer-create "*Hyde*"))
  (let (
	(posts (split-string (shell-command-to-string
			      (concat "cd " hyde-home "/" hyde-posts-dir " ; " hyde/hyde-list-posts-command )) "\n")))
    (dolist (post posts)
      (insert (concat post "\n"))
      )))

(defun hyde ()
  "Enters hyde mode"
  (interactive)
  )
  
(provide 'hyde)

