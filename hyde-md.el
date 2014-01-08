;;; hyde-md.el
;; Copyright (C) 2004 Noufal Ibrahim <noufal at nibrahim.net.in>
;;
;; This program is not part of Gnu Emacs
;;
;; hyde-md.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.
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

(defun  hyde-markdown-processp (asset)
  "Returns true if an asset is to be processed"
  (and (not (string-prefix-p "http://" asset))
       (not (string-prefix-p "https://" asset))
       (not (string-prefix-p hyde-images-dir asset))))

(defun hyde-markdown-process-assets ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "!\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
      (let ((asset (match-string-no-properties 2)))
        (message (format "Found : %s" asset))
        (if (hyde-markdown-processp asset)
            (progn
              ;; First copy over the asset properly to the images directory
              (let ((new-name (hyde-markdown-copy-over-asset asset hyde-home)))
                (message (format "Complete match is %s" (match-string-no-properties 0)))
                (message (format "Copied over to %s" new-name))
                ;; rewrite the URL in the markdown file
                (message (format "Replacing with %s" (format "![\\1](%s)" new-name)))
                (let ((p (copy-marker (point) t)))
                  (replace-match (format "![\\1](%s)" new-name))
                  (goto-char p)))))))))


(defun hyde-markdown-create-target-filename (sourcefile target_dir)
  (let* ((target_file (strip-string (shell-command-to-string (format "basename %s" sourcefile))))
         (target (format "%s/%s" target_dir target_file))
         (cntr 1))
    (progn
      (while (file-exists-p target)
        (setq target (format "%s/%d-%s" target_dir cntr target_file))
        (setq cntr (1+ cntr)))
      target)))
      
(defun hyde-markdown-copy-over-asset (asset hyde-home)
  (let (
        (full-target (hyde-markdown-create-target-filename asset hyde-images-dir))
        )
    (progn
      (copy-file asset full-target)
      (concat "/" 
              (replace-regexp-in-string (format "%s/?" (regexp-quote hyde-home)) "" full-target)))))



(defun hyde-markdown-end-edit ()
  "Function called signifying the end of the editing session"
  (interactive)
  (hyde-markdown-process-assets)
  (save-buffer (current-buffer))
  (hyde/vc-commit hyde-home 
                  (append (hyde/hyde-get-post-assets (buffer-file-name (current-buffer))) (list (buffer-file-name (current-buffer))))
                  (concat "Updating " (buffer-name (current-buffer))))
  (bury-buffer)
  (hyde/load-posts)
  nil)

(defun hyde-markdown-insert-image (image desc)
  (interactive "fImage file: 
sDescription: ")
  (insert (format "![%s](%s)" desc (expand-file-name image))))

(define-derived-mode hyde-markdown-mode  markdown-mode "Hyde-markdown" 
  "Markdown mode with a few extra bindings for convenience" 
  (define-key hyde-markdown-mode-map (kbd "C-c C-c") 'hyde-markdown-end-edit)
  (define-key hyde-markdown-mode-map (kbd "C-c C-i") 'hyde-markdown-insert-image))

(defun hyde-markdown-activate-mode (hyde-buffer)
  (hyde-markdown-mode)
  (set (make-local-variable 'hyde-home)
       (buffer-local-value 'hyde-home hyde-buffer)))

(provide 'hyde-md)

;;; hyde-md.el ends here
