;;; shm-nav.el --- Navigation commands

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

(require 'shm-macros)
(require 'shm-layout)

(defun shm/forward-paragraph ()
  "Go forward one declaration."
  (interactive)
  (unless (/= (point)
              (goto-char (cdr (shm-decl-points t))))
    (search-forward-regexp "[^\n ]" nil t 1)
    (backward-char)))

(defun shm/backward-paragraph ()
  "Go backward one declaration."
  (interactive)
  (unless (/= (point)
              (goto-char (car (shm-decl-points t))))
    (search-backward-regexp "[^\n ]" nil t 1)
    (forward-char)))

(defun shm/goto-where ()
  "Either make or go to a where clause of the current right-hand-side."
  (interactive)
  (let ((node-pair (shm-current-node-pair))
        (vector (shm-decl-ast)))
    (loop for i
          downfrom (car node-pair)
          to -1
          until (or (= i -1)
                    (let ((node (elt vector i)))
                      (and (string= "Rhs"
                                    (shm-node-type-name node))
                           (<= (shm-node-start node)
                               (shm-node-start (cdr node-pair)))
                           (>= (shm-node-end node)
                               (shm-node-end (cdr node-pair))))))
          finally (return
                   (when (>= i 0)
                     (let ((rhs (elt vector i)))
                       (goto-char (shm-node-end rhs))
                       (cond
                        ((looking-at "[\n ]*where")
                         (search-forward-regexp "where[ \n]*"))
                        (t
                         (unless (= (line-beginning-position) (point))
                           (insert "\n"))
                         (let ((indent (shm-node-start-column
                                        (cdr (shm-node-parent (cons i rhs))))))
                           (indent-to (+ 2 indent))
                           (insert "where")
                           (if shm-indent-point-after-adding-where-clause
                             (progn
                               (insert "\n")
                               (indent-to (+ 4 indent)))
                             (insert " ")))))))))))

(defun shm/goto-parent-end ()
  "Set the current node overlay to the parent node, but go to the
  end rather than the start."
  (interactive)
  (shm/goto-parent nil 'end))

(defun shm/forward-node ()
  "Go forward by node, i.e. go to the next of the current node. If
we're already at the end of the current node, jump to the next
node."
  (interactive)
  (let* ((current-pair (shm-current-node-pair))
         (current (cdr current-pair)))
    (if (= (point) (shm-node-end current))
        (let ((next-pair (shm-node-next current-pair)))
          (goto-char (shm-node-start (cdr next-pair))))
      (progn (goto-char (shm-node-end current))
             (setq shm-last-point (point))))))

(defun shm/backward-node ()
  "Go backward by node, i.e. go to the previous of the current node. If
we're already at the start of the current node, jump to the previous
node."
  (interactive)
  (let* ((current-pair (shm-current-node-pair))
         (current (cdr current-pair)))
    (if (= (point) (shm-node-start current))
        (let ((prev-pair (shm-node-previous current-pair)))
          (goto-char (shm-node-start (cdr prev-pair))))
      (progn (goto-char (shm-node-start current))
             (setq shm-last-point (point))))))

(defun shm/close-paren ()
  "Either insert a close paren or go to the end of the node."
  (interactive)
  (shm-with-fallback
   self-insert-command
   (if (shm-literal-insertion)
       (shm-insert-string ")")
     (progn (shm/reparse)
            (shm/goto-parent-end)))))

(defun shm/close-bracket ()
  "Either insert a close bracket or go to the end of the node."
  (interactive)
  (shm-with-fallback
   self-insert-command
   (if (shm-literal-insertion)
       (shm-insert-string "]")
     (progn (shm/reparse)
            (shm/goto-parent-end)))))

(defun shm/close-brace ()
  "Either insert a close brace or go to the end of the node."
  (interactive)
  (shm-with-fallback
   self-insert-command
   (if (shm-literal-insertion)
       (shm-insert-string "}")
     (progn (shm/reparse)
            (shm/goto-parent-end)))))

(provide 'shm-nav)
