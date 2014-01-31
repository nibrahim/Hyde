;;; hyde.el --- Major mode to help create and manage Jekyll blogs

;; Copyright (C) 2004 Noufal Ibrahim <noufal at nibrahim.net.in>
;;
;; This program is not part of Gnu Emacs
;;
;; hyde.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
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
(require 'easymenu)

;; Constants for internal use
(defconst hyde/hyde-version "0.3a"
  "Hyde version")

;; Internal customisable variables
(defcustom hyde-mode-hook nil
  "Hook called by \"hyde-mode\""
  :type 'hook
  :group 'hyde)


(defcustom hyde-deploy-dir
  "_site"
  "Directory which needs to be deployed"
  :type 'string
  :group 'hyde)


(defcustom hyde-posts-dir 
  "_posts"
  "Directory which contains the list of posts"
  :type 'string
  :group 'hyde)

(defcustom hyde-drafts-dir
  "_drafts"
  "Directory which contains post drafts"
  :type 'string
  :group 'hyde)

(defcustom hyde-images-dir
  "images"
  "Directory which contains images embedded on the blog"
  :type 'string
  :group 'hyde)


(defcustom hyde/jekyll-command
  "jekyll"
  "Command to run jekyll to create the blog"
  :type 'string
  :group 'hyde)

(defcustom hyde/serve-command
  "jekyll serve"
  "Command to serve jekyll to the localhost"
  :type 'string
  :group 'hyde)

(defvar hyde/serve-process nil "Process to keep track of serve")

(defcustom hyde/deploy-command
  "rsync -vr _site/* nkv@ssh.hcoop.net:/afs/hcoop.net/user/n/nk/nkv/public_html/nibrahim.net.in/"
  "Command used to deploy the site to the actual server"
  :type 'string
  :group 'hyde)

;; Faces and font-locking
(defface hyde-header-face
  '(
    (((type tty) (class color)) (:foreground "blue" :background "gray"))
    (((type graphic) (class color)) (:foreground "blue" :background "gray"))
    (t (:foreground "blue" :background "gray"))
    )
  "Face for a hyde header"
  :group 'hyde)

(defface hyde-committed-face
  '(
    (((type tty) (class color)) (:foreground "blue"))
    (((type graphic) (class color)) (:foreground "blue"))
    (t (:foreground "blue"))
    )
  "Face for a file that has been committed"
  :group 'hyde)


(defface hyde-modified-face
  '(
    (((type tty) (class color)) (:foreground "red"))
    (((type graphic) (class color)) (:foreground "red"))
    (t (:foreground "red"))
    )
  "Face for a file that has been modified but not committed"
  :group 'hyde)

(defface hyde-unsaved-face
  '(
    (((type tty) (class color)) (:foreground "black" :background "red"))
    (((type graphic) (class color)) (:foreground "black" :background "red"))
    (t (:foreground "black" :background "red"))
    )
  "Face for a file that has been modified but not even saved"
  :group 'hyde)

(defface hyde-pushed-face
  '(
    (((type tty) (class color)) (:foreground "green"))
    (((type graphic) (class color)) (:foreground "green"))
    (t (:foreground "green"))
    )
  "Face for a file that has been pushed to the remote repo"
:group 'hyde)

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
(defalias 'hyde/vc-rename  'hyde/git/rename "Command to rename files")

(defun hyde/hyde-file-local-uncommitted-changed (dir file)
  "Return whether the given file in the given dir is uncommitted"
  (hyde/vc-uncommittedp (concat hyde-home "/" dir) file))

(defun hyde/hyde-file-committed-not-pushed (dir file)
  "Return whether the given file in the given dir is unpushed"
  (hyde/vc-unpushedp (concat hyde-home "/" dir) file))

(defun hyde/hyde-file-committed-pushed (dir file)
  "Return whether the given file in the given dir is pushed"
  (hyde/vc-pushedp (concat hyde-home "/" dir) file))

(defun hyde/hyde-add-file (file)
  "Stages the given file for commit."
  (hyde/vc-add (concat hyde-home "/" hyde-posts-dir) file))

(defun hyde/hyde-rename-file (from to)
  "Renames the given version controlled file from to to"
  (hyde/vc-rename hyde-home from to))

(defun hyde/hyde-commit-post (pos commit-message)
  "Commits the changes in the repository"
  (interactive "d\nMCommit message : ")
  (let* (
         (post-file-name (nth 
                          1
                          (split-string (strip-string (thing-at-point 'line)) " : ")))
         (dir (get-text-property pos 'dir))
         (post-full-path (concat hyde-home "/" dir "/" post-file-name))
         )
    (hyde/vc-commit (concat hyde-home "/" dir)
                    (append (hyde/hyde-get-post-assets post-full-path) (list post-file-name))
                    commit-message)
    (hyde/load-posts)))

(defun hyde/hyde-push ()
  "Publishes the changes to the remote repository"
  (interactive)
  (hyde/vc-push hyde-home)
  (hyde/load-posts))

(defun hyde/run-jekyll ()
  "Runs jekyll on the directory"
  (interactive)
  (shell-command (format "cd %s && %s" (expand-file-name hyde-home) hyde/jekyll-command)))

(defun hyde/stop-serve ()
  "Stops jekyll serve if running"
  (interactive)
  (when hyde/serve-process
    (delete-process hyde/serve-process)
    (setq hyde/serve-process nil)))

(defun hyde/serve ()
  "Serves jekyll to localhost in an asynchronous process. If
already started, stops and restarts."
  (interactive)
  (hyde/stop-serve)
  (setq hyde/serve-process
   (start-process-shell-command "hyde/serve" "*hyde/serve*"
    (format "cd %s && %s" (expand-file-name hyde-home) hyde/serve-command))))

(defun hyde/deploy ()
  "Deploys the generated website (should be run after hyde/run-jekyll"
  (interactive)
  (shell-command (format "cd %s && %s" (expand-file-name hyde-home) hyde/deploy-command)))
  
  
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
  (let* (
         (posts-dir (concat (expand-file-name hyde-home) "/" dir))
         (posts (directory-files posts-dir nil ".*md\\|.*markdown" nil)))
    (map 'list (lambda (f) (format "%s : %s" (hyde/file-status dir f) f)) posts)))

(defun hyde/hyde-get-post-assets (post)
  (save-excursion
    (with-current-buffer (find-file post)
      (goto-char (point-min))
      (let ((assets '()))
        (while (re-search-forward "!\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
          ;; TBD don't try to process http assets.
          (add-to-list 'assets (concat
                                (strip-string (shell-command-to-string (format "dirname %s" post)))
                                "/"
                                (match-string-no-properties 2))))
        assets))))
  
(defun hyde/promote-to-post (pos)
  "Promotes the post under the cursor from a draft to a post"
  (interactive "d")
  (let (
	(post-file-name (nth 
			 1
			 (split-string (strip-string (thing-at-point 'line)) " : ")))
	(dir (get-text-property pos 'dir)))
    (if (equal dir hyde-drafts-dir)
        (progn
          ;; Move over post assets
          (dolist (asset (hyde/hyde-get-post-assets (concat dir "/" post-file-name)))
            (progn
              (message (concat "Asset is : " asset))
              (hyde/hyde-rename-file asset 
                                     (format "%s%s" hyde-home
                                             (replace-regexp-in-string "_drafts" "" asset)))))
          ;; Move over the actual post
          (hyde/hyde-rename-file (concat dir "/" post-file-name)
                                 (concat hyde-posts-dir "/" post-file-name))))
    (hyde/vc-commit hyde-home
                    '()
                    (concat "Promoting " post-file-name))
    (hyde/load-posts)))


(defun hyde/open-post-maybe (pos)
  "Opens the post under cursor in the editor"
  (interactive "d")
  (let (
	(post-file-name (nth 
			 1
			 (split-string (strip-string (thing-at-point 'line)) " : ")))
	(dir (get-text-property pos 'dir)))
    (let ((hyde-buffer (current-buffer)))
      (find-file 
       (strip-string (concat hyde-home "/" dir "/" post-file-name)))
      (hyde-markdown-activate-mode hyde-buffer))))


(defun hyde/new-post (title)
  "Creates a new post"
  (interactive "MEnter post title: ")
  (let ((post-file-name (expand-file-name (format "%s/%s/%s.markdown" 
                                                  hyde-home hyde-drafts-dir (concat 
                                                                             (format-time-string "%Y-%m-%d-")
                                                                             (downcase (replace-regexp-in-string " " "_" title))))))
        (hyde-buffer (current-buffer)))
    (save-excursion
      (find-file post-file-name)
      (insert "---\n")
      (insert "layout: post\n")
      (insert (format "title: \"%s\"\n" title))
      (insert (format "date: \"%s\"\n" (format-time-string "%Y-%m-%d %H:%M:%S %z")))
      (insert "---\n\n")
      (save-buffer))
    (hyde/hyde-add-file post-file-name)
    (find-file post-file-name)

     ;; hyde-home not available in markdown buffer (FIXME)
    (hyde-markdown-activate-mode hyde-buffer)))

(defun hyde/quit ()
  "Quits hyde"
  (interactive)
  (kill-buffer (current-buffer)))


;; Keymaps
(defvar hyde-mode-map
  (let 
      ((hyde-mode-map (make-sparse-keymap)))
    (define-key hyde-mode-map (kbd "n") 'hyde/new-post)
    (define-key hyde-mode-map (kbd "g") 'hyde/load-posts)
    (define-key hyde-mode-map (kbd "c") 'hyde/hyde-commit-post)
    (define-key hyde-mode-map (kbd "P") 'hyde/hyde-push)
    (define-key hyde-mode-map (kbd "j") 'hyde/run-jekyll)
    (define-key hyde-mode-map (kbd "s") 'hyde/serve)
    (define-key hyde-mode-map (kbd "k") 'hyde/stop-serve)
    (define-key hyde-mode-map (kbd "d") 'hyde/deploy)
    (define-key hyde-mode-map (kbd "p") 'hyde/promote-to-post)
    (define-key hyde-mode-map (kbd "q") 'hyde/quit)
    (define-key hyde-mode-map (kbd "RET") 'hyde/open-post-maybe)
    hyde-mode-map)
  "Keymap for Hyde")

;; Menu
(easy-menu-define hyde-mode-menu hyde-mode-map
  "Hyde menu"
  '("Hyde"
    ["New post" hyde/new-post t]
    ["Open post" hyde/open-post-maybe t]
    ["Commit post" hyde/hyde-commit-post t]
    ["Promote post" hyde/promote-to-post t]
    "---"
    ["Refresh" hyde/load-posts t]
    ["Run Jekyll" hyde/run-jekyll t]
    ["(Re)start server" hyde/serve t]
    ["Stop server" hyde/stop-serve t]
    "---"
    ["Deploy" hyde/deploy t]
    ["Push" hyde/hyde-push t]
    ["Quit" hyde/quit t]
    ))

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
  ;; Inserts post for the drafts directory
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

(defun hyde/read-config (hyde-home)
  "Loads up the config file to set the blog deployment and other information"
  (let (
	(config-file (concat hyde-home "/.hyde.el"))
	)
    (if (not (file-exists-p config-file))
        (error (format "Config file '%s' is missing. Won't continue" config-file)))
    (message (format "Loading %s" config-file))
    (load-file config-file)
    ))
  
(defun hyde/setup-directories (home)
  "Create expected directories if they don't exist"
  (let
      (
       (drafts-dir (concat home "/" hyde-drafts-dir))
       )
    (if (not (file-exists-p drafts-dir))
        (make-directory drafts-dir t))))

(defun hyde/hyde-mode (home)
  "The Hyde major mode to edit Jekyll posts.

\\{hyde-mode-map}"
  (kill-all-local-variables)
  (dolist (x '(hyde-deploy-dir
	       hyde-posts-dir
	       hyde-drafts-dir
	       hyde/jekyll-command
	       hyde/deploy-command
	       hyde/git/remote
	       hyde/git/remote-branch))
    (make-variable-buffer-local x))
  (set (make-local-variable 'hyde-home) home)
  (use-local-map hyde-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(hyde-font-lock-keywords))
  (setq major-mode 'hyde/hyde-mode
	mode-name "Hyde"
        default-directory home)
  (hyde/read-config hyde-home)
  (hyde/setup-directories hyde-home)
  (hyde/load-posts)
  (hl-line-mode t)
  ;; Create directories for images
  (let ((draft-images-dir (concat hyde-home hyde-drafts-dir "/" hyde-images-dir))
        (posts-images-dir (concat hyde-home "/" hyde-images-dir)))
    (progn
      (message (concat "Drafts image dir :"draft-images-dir))
      (message (concat "Posts image dir :"posts-images-dir))
      (if (not (file-exists-p draft-images-dir))
          (make-directory draft-images-dir))
      (if (not (file-exists-p posts-images-dir))
          (make-directory posts-images-dir))))
  (run-hooks hyde-mode-hook))


;; Entry point
(defun hyde (home)
  "Enters hyde mode"
  (interactive "DBlog : ")
  (let (
	(hyde-buffer (concat "*Hyde : " home "*"))
	)
    (switch-to-buffer (get-buffer-create hyde-buffer)))
  (hyde/hyde-mode home))

(provide 'hyde)

;;; hyde.el ends here
