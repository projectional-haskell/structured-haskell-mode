;;; shm-yank-kill.el --- Killing and yanking operations

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


;; Requirements

(require 'shm-macros)

(defun shm/kill-region (beg end)
  "Kill the region, and save it in the clipboard."
  (interactive "r")
  (shm-kill-region nil beg end nil))

(defun shm/copy-region (beg end)
  "Copy the region, and save it in the clipboard."
  (interactive "r")
  (save-excursion
    (shm-kill-region 'clipboard-kill-ring-save beg end t)))

(defun shm/kill-line ()
  "Kill everything possible to kill after point before the end of
the line.

Successive kills will also work, for example:

do |foo
   bar
   mu

Hitting C-k C-k C-k here will killall three lines, and then C-y
will insert them back verbatim."
  (interactive)
  (shm-with-fallback
   kill-line
   (shm/reparse)
   (cond
    ((looking-at "^[ ]+$")
     (delete-region (point) (line-end-position)))
    ((= (line-end-position) (line-beginning-position))
     (delete-char -1)
     (forward-char 1))
    ((and (shm-in-string)
          (not (= (point)
                  (shm-node-start (shm-current-node)))))
     (let ((current (shm-current-node)))
       (if (and (> (shm-node-end current)
                   (line-end-position))
                (save-excursion (goto-char (line-end-position))
                                (looking-back "\\\\")))
           (kill-region (point) (1- (line-end-position)))
         (kill-region (point)
                      (1- (shm-node-end current))))))
    ((and (= (point) (line-end-position))
          (not (looking-at "\n[^ ]")))
     (let ((column (current-column)))
       (delete-region (point)
                      (save-excursion (forward-line 1)
                                      (goto-char (+ (line-beginning-position)
                                                    column))))
       (shm-kill-to-end-of-line t)))
    ((shm-current-node) (shm-kill-to-end-of-line))
    (t (kill-line)))))

(defun shm/kill-node ()
  "Kill the current node."
  (interactive)
  (shm-kill-node))

(defun shm/yank ()
  "Yank from the kill ring and insert indented with `shm-insert-indented'."
  (interactive)
  (shm-with-fallback
   yank
   ;; This avoids merging two identifiers together accidentally.
   (unless (or (shm-in-comment)
               (shm-in-string))
     (when (looking-back "[a-zA-Z0-9]+_*")
       (shm-insert-string " "))
     (when (looking-at "[a-zA-Z0-9]+_*")
       (shm-insert-string " ")
       (forward-char -1)))
   (shm-insert-indented #'clipboard-yank)))

(defun shm/yank-pop ()
  "Yank from the kill ring and insert indented with `shm-insert-indented'."
  (interactive)
  (shm-with-fallback
   yank-pop
   (if (not (eq last-command 'yank))
       (error "Previous command was not a yank (error from shm/yank-pop)"))
   (apply #'delete-region shm-last-yanked)
   (shm-insert-indented #'yank-pop)))

(defun shm-kill-node (&optional save-it node start do-not-delete)
  "Kill the current node.

See documentation of `shm-kill-region' for the transformations
this does."
  (interactive)
  (let* ((current (or node (shm-current-node))))
    (shm-kill-region save-it
                     (or start (shm-node-start current))
                     (shm-node-end current)
                     do-not-delete)))

(defun shm-kill-region (save-it start end do-not-delete)
  "Kill the given region, dropping any redundant indentation.

This normalizes everything it kills assuming what has been killed
is a node or set of nodes. Indentation is stripped off and
preserved appropriately so that if we kill e.g.

foo = {do bar
          mu}

where {} indicates the current node, then what is put into the kill ring is:

do bar
   mu

rather than what is normally put there,

do bar
          mu

So this is nice to paste elsewhere outside of Emacs, but it's
especially nice for pasting back into other parts of code,
because the yank function will take advantage of this
normalization and paste and re-indent to fit into the new
location. See `shm/yank' for documentation on that."
  (goto-char start)
  (let* ((start-col (current-column))
         (multi-line (/= (line-beginning-position)
                         (save-excursion (goto-char end)
                                         (line-beginning-position))))
         (string (buffer-substring-no-properties
                  start
                  end))
         (result
          (unless (string= string "")
            (with-temp-buffer
              (erase-buffer)
              (when multi-line
                (insert (make-string start-col ? )))
              (insert string)
              ;; This code de-indents code until a single line is hitting column zero.
              (let ((indent-tabs-mode nil))
		(while (progn (goto-char (point-min))
			      (not (and (search-forward-regexp "^[^ ]" nil t 1)
					(forward-line -1)
					;; If there are empty lines, they
					;; don't count as hitting column zero.
					(if (/= (line-beginning-position)
						(line-end-position))
					    t
					  ;; And we should actually delete empty lines.
					  (progn (if (bobp)
                                                     (delete-region (point) (1+ (point)))
                                                   (delete-region (1- (point)) (point)))
						 nil)))))
		  ;; Bring everything back one.
		  (indent-rigidly (point-min) (point-max)
				  -1)))
              ;; If there's an empty line at the end, then strip that
              ;; out. It's just bothersome when pasting back in.
              (goto-char (point-max))
              (when (looking-at "^$")
                (delete-region (1- (point))
                               (point)))
              ;; Finally, the actual save.
              (funcall (if save-it save-it 'clipboard-kill-ring-save)
                       (point-min)
                       (point-max))))))
    (let ((inhibit-read-only t))
      (unless do-not-delete
        (delete-region start
                       end)))
    result))

(defun shm-kill-to-end-of-line (&optional prepend-newline)
  "Kill everything possible to kill after point before the end of
the line."
  (let* ((vector (shm-decl-ast))
         (current-pair (shm-current-node-pair))
         (current (cdr current-pair))
         (parent-pair (shm-node-ancestor-at-point current-pair (point)))
         (parent (cdr parent-pair)))
    (loop for i
          from 0
          to (length vector)
          until (or (>= i (length vector))
                    (let ((node (elt vector i)))
                      (and (>= (shm-node-start node)
                               (shm-node-start parent))
                           (<= (shm-node-end node)
                               (shm-node-end parent)))))
          finally (return
                   (let ((last-command (if prepend-newline 'kill-region last-command)))
                     (when prepend-newline
                       (kill-append "\n" nil))
                     (if (< i (length vector))
                         (shm-kill-node 'clipboard-kill-ring-save
                                        parent
                                        (point))
                       (let ((line-end-position (if prepend-newline
                                                    (save-excursion (forward-line)
                                                                    (line-end-position))
                                                  (line-end-position))))
                         (when (= (point)
                                  line-end-position)
                           (kill-region (point)
                                        line-end-position)))))))))

(provide 'shm-yank-kill)
