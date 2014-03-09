;;; shm-constraint.el --- Constraint editing functions.

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

(require 'shm-ast)

(defun shm/modify-type-constraint ()
  "Modify a type signatures constraint"
  (interactive)
  (let* ((pair (shm-current-node-pair))
         (current-node (cdr pair)))
    (if (shm-type-signature-with-constraint-p pair)
        (shm-add-additional-type-constraint current-node)
      (add-initial-type-constraint current-node))))

(defun shm-add-additional-type-constraint (node)
  (if (shm-constraint-has-parens-p node)
      (progn
        (shm-goto-end-of-constraint node)
        (backward-char 1)
        (insert ", "))
    (goto-char (shm-node-start node))
    (insert "(")
    (shm-goto-end-of-constraint node)
    (insert ", )")
    (backward-char 1)))

(defun add-initial-type-constraint (node)
  (goto-char (shm-node-start node))
  (insert " => ") (backward-char 4))

(defun shm-top-level-type-decl-p (node-pair)
  (let ((current-node (cdr node-pair)))
    (if (and (not (shm-has-parent-with-matching-type-p node-pair))
             (string= "Type SrcSpanInfo" (shm-node-type current-node))) t)))

(defun shm-type-signature-with-constraint-p (pair)
  (let ((current-node (cdr pair)))
    (and (shm-top-level-type-decl-p pair)
         (shm-node-syntax-contains-regex "=>" current-node))))

(defun shm-constraint-has-parens-p (node)
   (let* ((syntax (shm-concrete-syntax-for-node node))
          (constraint-syntax (car (split-string syntax "=>"))))
     (string-match-p ")" constraint-syntax)))

(defun shm-goto-end-of-constraint (node)
  "Set point to the first white-space character between the end of the type constraint and the '=>'"
  (goto-char (+ (shm-node-start node)
                (shm-node-syntax-contains-regex "=>" node)))
  (re-search-backward "^\\|[^[:space:]]") (goto-char (+ (point) 1)))

(defun shm-node-syntax-contains-regex (regex node)
  "check the syntax of a node for an occurrence of pattern"
  (let ((node-concrete-syntax (shm-concrete-syntax-for-node node)))
    (string-match-p regex node-concrete-syntax)))

(defun shm-concrete-syntax-for-node (node)
  "Get the concrete syntax of the node"
  (buffer-substring-no-properties
   (shm-node-start (shm-current-node))
   (shm-node-end (shm-current-node))))

(defun shm-has-parent-with-matching-type-p (node-pair)
  (let* ((current (cdr node-pair))
         (parent-pair (shm-node-parent node-pair (shm-node-type current)))
         (parent (cdr parent-pair)))
    (if parent
        (if (string= (shm-node-type current)
                     (shm-node-type parent)) t))))

(provide 'shm-constraint)
