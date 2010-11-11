;;; hyde-md.el
;; Copyright (C) 2004 Noufal Ibrahim <noufal at nibrahim.net.in>
;;
;; This program is not part of Gnu Emacs
;;
;; hyde-md.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2 of the License,
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

(defun hyde-markdown-end-edit ()
  "Function called signifying the end of the editing session"
  (interactive)
  (save-buffer (current-buffer))
  (switch-to-buffer (get-buffer-create "*Hyde*"))
  (hyde/load-posts)
  nil)


(define-derived-mode hyde-markdown-mode  markdown-mode "Hyde-markdown" 
  "Markdown mode with a few extra bindings for convenience" 
  (define-key hyde-markdown-mode-map (kbd "C-c C-c") 'hyde-markdown-end-edit))
  (define-key hyde-markdown-mode-map (kbd "C-c C-v") 'markdown-preview)
(provide 'hyde-md)

