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

;; Requirements
(require 'hyde-git)
(require 'hyde-md)

;; Constants for internal use
(defconst hyde/hyde-version "0.1" 
  "Hyde version")

;; Internal customisable variables
(defvar hyde-mode-hook nil
  "Hook called by \"hyde-mode\"")

(defcustom hyde-deploy-dir
  "_site"
  "Directory which needs to be deployed")

(defcustom hyde-posts-dir 
  "_posts"
  "Directory which contains the list of posts")

(defcustom hyde-drafts-dir
  "_drafts"
  "Directory which contains post drafts")

(defcustom hyde/hyde-list-posts-command 
  "/bin/ls -1tr "
  "Command to list the posts")

(defcustom hyde/jekyll-command
  "jekyll"
  "Command to run jekyll to create the blog")

(defcustom hyde/deploy-command
  "rsync -vr _site/* nkv@ssh.hcoop.net:/afs/hcoop.net/user/n/nk/nkv/public_html/nibrahim.net.in/"
  "Command used to deploy the site to the actual server")

;; Faces and font-locking
(defface hyde-header-face
  '(
    (((type tty) (class color)) (:foreground "blue" :background "gray"))
    (((type graphic) (class color)) (:foreground "blue" :background "gray"))
    (t (:foreground "blue" :background "gray"))
    )
  "Face for a hyde header")

(defface hyde-committed-face
  '(
    (((type tty) (class color)) (:foreground "blue"))
    (((type graphic) (class color)) (:foreground "blue"))
    (t (:foreground "blue"))
    )
  "Face for a file that has been committed")

(defface hyde-modified-face
  '(
    (((type tty) (class color)) (:foreground "red"))
    (((type graphic) (class color)) (:foreground "red"))
    (t (:foreground "red"))
    )
  "Face for a file that has been modified but not committed")

(defface hyde-unsaved-face
  '(
    (((type tty) (class color)) (:foreground "black" :background "red"))
    (((type graphic) (class color)) (:foreground "black" :background "red"))
    (t (:foreground "black" :background "red"))
    )
  "Face for a file that has been modified but not even saved")

(defface hyde-pushed-face
  '(
    (((type tty) (class color)) (:foreground "green"))
    (((type graphic) (class color)) (:foreground "green"))
    (t (:foreground "green"))
    )
  "Face for a file that has been pushed to the remote repo")

(defvar hyde-header-face 'hyde-header-face "Face for a hyde header")
(defvar hyde-committed-face 'hyde-committed-face)
(defvar hyde-modified-face 'hyde-modified-face)
(defvar hyde-unsaved-face  'hyde-unsaved-face )
(defvar hyde-pushed-face 'hyde-pushed-face)

(defconst hyde-font-lock-keywords
  (list
   '("^::.*" . hyde-header-face)
   '("^C :.*" . hyde-committed-face)
   '("^M :.*" . hyde-modified-face)
   '("^E :.*" . hyde-unsaved-face)
   '("^\\. :.*" . hyde-pushed-face)
   )
  "Font lock keywords for Hyde mode")

;; Version control abstraction
(defalias 'hyde/vc-uncommittedp 'hyde/git/uncommittedp "Command to check whether a file has uncommitted changes")
(defalias 'hyde/vc-unpushedp 'hyde/git/unpushedp "Command to check whether a file has unpushed changes")
(defalias 'hyde/vc-pushedp  'hyde/git/pushedp "Command to check whether a file has pushed changes")
(defalias 'hyde/vc-add  'hyde/git/add "Command to add a file to the DVCS")
(defalias 'hyde/vc-commit  'hyde/git/commit "Command to add a file to the DVCS")
(defalias 'hyde/vc-push  'hyde/git/push "Command to push the repository")
(defalias 'hyde/vc-rename  'hyde/git/rename "Command to push the repository")

(defun hyde/hyde-file-local-uncommitted-changed (dir file)
  (hyde/vc-uncommittedp (concat hyde-home "/" dir) file))

(defun hyde/hyde-file-committed-not-pushed (dir file)
  (hyde/vc-unpushedp (concat hyde-home "/" dir) file))

(defun hyde/hyde-file-committed-pushed (dir file)
  (hyde/vc-pushedp (concat hyde-home "/" dir) file))

(defun hyde/hyde-add-file (file)
  (hyde/vc-add (concat hyde-home "/" hyde-posts-dir) file))

(defun hyde/hyde-rename-file (from to)
  (hyde/vc-rename hyde-home from to))

(defun hyde/hyde-commit-post (pos commit-message)
  (interactive "d\nMCommit message : ")
  (let (
	(post-file-name (nth 
			 1
			 (split-string (strip-string (thing-at-point 'line)) " : ")))
	(dir (get-text-property pos 'dir)))
    (hyde/vc-commit (concat hyde-home "/" dir) post-file-name commit-message)
    (hyde/load-posts)))

(defun hyde/hyde-push ()
  (interactive)
  (hyde/vc-push hyde-home)
  (hyde/load-posts))

(defun hyde/run-jekyll ()
  (interactive)
  (shell-command (format "cd %s && %s" hyde-home hyde/jekyll-command)))

(defun hyde/deploy ()
  (interactive)
  (shell-command (format "cd %s && %s" hyde-home hyde/deploy-command)))
  
  
;; Utility functions
(defun hyde/hyde-file-local-unsaved-changed (dir file)
  "Returns true if and only if the given file contains unsaved changes"
    (let (
	(buffer (get-file-buffer file))
	)
    (if buffer
	(buffer-modified-p buffer)
      nil)))

(defun strip-string (str)
  "Returns STR with all trailing whitespaces gone"
  (replace-regexp-in-string "\n$" "" str))

(defun hyde/file-status (dir file)
  "Returns an letter indicating the status of the file as far as
hyde is concerned

Committed means that the changes have been committed into your DVCS
Pushed out means that they have been pushed to a safe remote repo (github, bitbucket etc.)

Status indicators are as follows:

. Committed and pushed
C Committed but not yet pushed
M Local saved changes (uncommitted)
E Local unsaved changes"
  (or 
   (and (hyde/hyde-file-local-unsaved-changed dir file) "E")
   (and (hyde/hyde-file-local-uncommitted-changed dir file) "M")
   (and (hyde/hyde-file-committed-not-pushed dir file) "C")
   (and (hyde/hyde-file-committed-pushed dir file) ".")))


(defun hyde/list-format-posts (dir)
  "Gets the lists of posts from the given directory, formats them
properly and returns them so that they can be presented to the
user"
  (let (
	(posts (split-string (strip-string (shell-command-to-string
					    (concat "cd " hyde-home "/" dir " ; " hyde/hyde-list-posts-command ))))))
    (map 'list (lambda (f) (format "%s : %s" (hyde/file-status dir f) f)) posts)))

(defun hyde/promote-to-post (pos)
  (interactive "d")
  (let (
	(post-file-name (nth 
			 1
			 (split-string (strip-string (thing-at-point 'line)) " : ")))
	(dir (get-text-property pos 'dir)))
    (if (equal dir hyde-drafts-dir)
	(hyde/hyde-rename-file (concat dir "/" post-file-name)
			       (concat hyde-posts-dir "/" post-file-name)))
    (hyde/load-posts)))


(defun hyde/open-post-maybe (pos)
  (interactive "d")
  (let (
	(post-file-name (nth 
			 1
			 (split-string (strip-string (thing-at-point 'line)) " : ")))
	(dir (get-text-property pos 'dir)))
    (find-file 
     (strip-string (concat hyde-home "/" dir "/" post-file-name)))
    (hyde-markdown-mode)))

(defun hyde/new-post (title)
  (interactive "MEnter post title: ")
  (let ((post-file-name (format "%s/%s/%s.markdown" 
				hyde-home hyde-drafts-dir (concat 
							  (format-time-string "%Y-%m-%d-")
							  (downcase (replace-regexp-in-string " " "_" title))))))

    (find-file post-file-name)
    (insert "---\n")
    (insert "layout: post\n")
    (insert (format "title: \"%s\"\n" title))
    (insert "---\n\n")
    (save-buffer)
    (hyde/hyde-add-file post-file-name)
    (hyde-markdown-mode)))

;; Keymaps
(defvar hyde-mode-map
  (let 
      ((hyde-mode-map (make-sparse-keymap)))
    (define-key hyde-mode-map (kbd "n") 'hyde/new-post)
    (define-key hyde-mode-map (kbd "g") 'hyde/load-posts)
    (define-key hyde-mode-map (kbd "c") 'hyde/hyde-commit-post)
    (define-key hyde-mode-map (kbd "P") 'hyde/hyde-push)
    (define-key hyde-mode-map (kbd "j") 'hyde/run-jekyll)
    (define-key hyde-mode-map (kbd "d") 'hyde/deploy)
    (define-key hyde-mode-map (kbd "p") 'hyde/promote-to-post)
    (define-key hyde-mode-map (kbd "q") '(lambda () (interactive) (kill-buffer (current-buffer))))
    (define-key hyde-mode-map (kbd "RET") 'hyde/open-post-maybe)
    hyde-mode-map)
  "Keymap for Hyde")


(defun hyde/load-posts ()
  "Load up the posts and present them to the user"
  (interactive)
  ;; Clear the buffer
  (toggle-read-only -1)
  (delete-region (point-min) (point-max))
  ;; Insert headers
  (insert ":: Editing blog at:" hyde-home "\n")
  (insert ":: Posts\n")
  ;; Insert posts from posts directory
  (let 
      ((posts (hyde/list-format-posts hyde-posts-dir)))
    (dolist (post posts)
      (progn
	(save-excursion
	  (insert (concat post "\n")))
	(put-text-property (point) (+ (point) (length post)) 'dir hyde-posts-dir)
	(forward-line))))
    (insert "\n:: Drafts\n")
    (let 
	((posts (hyde/list-format-posts hyde-drafts-dir)))
      (dolist (post posts)
	(progn
	  (save-excursion
	    (insert (concat post "\n")))
	  (put-text-property (point) (+ (point) (length post)) 'dir hyde-drafts-dir)
	  (forward-line))))
    ;; Insert footer
    (insert (concat "\n\n:: Hyde version " hyde/hyde-version "\n"))
    (insert "Key:\n-----\n . Committed and pushed\n C Committed but not yet pushed\n M Local saved changes (uncommitted)\n E Local unsaved changes\n")
    (toggle-read-only 1))

(defun hyde/read-config ()
  "Loads up the config file to set the blog deployment and other information"
  (let (
	(config-file (concat hyde-home "/.hyde.el"))
	)
    (load-file config-file)
    ))
  

(defun hyde/hyde-mode ()
  "The Hyde major mode to edit Jekyll posts."
  (kill-all-local-variables)
  (use-local-map hyde-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(hyde-font-lock-keywords))
  (setq major-mode 'hyde/hyde-mode
	  mode-name "Hyde")
  (run-hooks hyde-mode-hook)
  (hl-line-mode t))

;; Entry point
(defun hyde (hyde-home)
  "Enters hyde mode"
  (interactive "DBlog :")
  (dolist (x '(hyde-home hyde-deploy-dir hyde-posts-dir hyde-drafts-dir hyde/hyde-list-posts-command hyde/jekyll-command hyde/deploy-command))
    (make-local-variable x))
  (let (
	(hyde-buffer (concat "*Hyde : " hyde-home "*"))
	)
    (switch-to-buffer (get-buffer-create hyde-buffer))
    (hyde/read-config))
  (hyde/load-posts)
  (hyde/hyde-mode))
  
(provide 'hyde)




