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

(defconst hyde/hyde-version "0.1" 
  "Hyde version")

(defvar hyde-mode-hook nil
  "Hook called by \"hyde-mode\"")


(defvar hyde-mode-map
  (let 
      ((hyde-mode-map (make-keymap)))
    ;; (define-key hyde-mode-map "\C-j" 'newline-and-indent)
    hyde-mode-map)
  "Keymap for Cisco router configuration major mode")

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

(defface hyde-header-face
  '(
    (((type tty) (class color)) (:foreground "blue" :weight 'bold))
    (((type graphic) (class color)) (:foreground "lightsteelblue" :weight 'bold))
    (t (:foreground "lightsteelblue" :weight "bold"))
    )
  "Face for a hyde header")

(defvar hyde-header-face 'hyde-header-face "Face for a hyde header")

(defconst hyde-font-lock-keywords
  (list
   '("^::.*" . hyde-header-face)
   )
  "Font lock keywords for Hyde mode")
  

(defun hyde/load-posts ()
  "Load up the posts and present them to the user"
  (interactive)
  ;; Insert headers
  (insert ":: Editing blog at:" hyde-home "\n")
  ;; Insert posts
  (save-excursion
    (let (
	  (posts (split-string (shell-command-to-string
				(concat "cd " hyde-home "/" hyde-posts-dir " ; " hyde/hyde-list-posts-command )) "\n")))
      (dolist (post posts)
	(insert (concat post "\n"))))
    ;; Insert footer
    (insert (concat ":: Hyde version " hyde/hyde-version "\n"))))

(defun hyde/hyde-mode ()
  "The Hyde major mode to edit Jekyll posts. I love how that sounds :)"
  (interactive)
  (kill-all-local-variables)
  (use-local-map hyde-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(hyde-font-lock-keywords))
  (setq major-mode 'hyde-mode
	  mode-name "Hyde")
  (run-hooks hyde-mode-hook))

(defun hyde ()
  "Enters hyde mode"
  (interactive)
  (switch-to-buffer (get-buffer-create "*Hyde*"))
  (hyde/hyde-mode)
  (hyde/load-posts))
  
(provide 'hyde)

