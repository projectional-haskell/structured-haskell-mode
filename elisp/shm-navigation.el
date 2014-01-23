;;; shm-navigation.el --- Navigation functions that don't change the buffer

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
      (goto-char (shm-node-end current)))))

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
      (goto-char (shm-node-start current)))))

(defun shm/goto-parent (&optional node-pair direction)
  "Set the current node overlay to the parent node-pair"
  (interactive)
  (let ((direction (or direction 'start)))
    (if shm-current-node-overlay
        (let* ((o shm-current-node-overlay)
               (parent-pair (shm-node-parent (or node-pair
                                                 (shm-current-workable-node)))))
          (when parent-pair
            (let ((parent (cdr parent-pair)))
              (if (and o
                       (overlay-buffer o)
                       (>= (shm-node-start parent)
                           (overlay-start o))
                       (<= (shm-node-end parent)
                           (overlay-end o)))
                  (shm/goto-parent parent-pair direction)
                (shm-set-node-overlay parent-pair direction)))))
      (when node-pair
        (shm-set-node-overlay node-pair direction)))))

(provide 'shm-navigation)
