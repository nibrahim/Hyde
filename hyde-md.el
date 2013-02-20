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

(defun hyde-markdown-extract-images (buffer)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((assets '()))
        (while (re-search-forward "!\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
          (add-to-list 'assets (match-string-no-properties 2)))
        assets ))))


(defun hyde-markdown-extract-and-add-images ()
  (dolist (asset (hyde-markdown-extract-images (current-buffer)))
    (copy-file asset (format "/%s/%s/%s"
                             hyde-home
                             hyde-images-dir 
                             (strip-string (shell-command-to-string (format "basename %s" asset)))) 1 )))


(defun hyde-markdown-end-edit ()
  "Function called signifying the end of the editing session"
  (interactive)
  (save-buffer (current-buffer))
  (hyde-markdown-extract-and-add-images)
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

