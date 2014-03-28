;;; shm-indent.el --- Indentation commands

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

(require 'shm-layout)

(defun shm/delete-indentation ()
  "Send the node up one line."
  (interactive)
  (if (looking-back "^[ ]+")
      (cond
       ((or (looking-at "then[] [{}\"'()]")
            (looking-at "else[] [{}\"'()]"))
        (delete-indentation))
       ((looking-at "[ ]*$")
        (delete-indentation))
       (t (let ((string (shm-kill-node 'buffer-substring-no-properties)))
            (delete-indentation)
            (insert " ")
            (shm-insert-indented
             (lambda ()
               (insert string))))))
    (delete-indentation)))

(defun shm/swing-down ()
  "Swing the children of the current node downwards.

hai = do foo bar
         mu zot

With the cursor on `do', this will produce:

hai = do
  foo bar
  mu zot
"
  (interactive)
  (let* ((current-pair (shm-current-node-pair))
         (current (cdr current-pair)))
    (cond
     ((eq (shm-node-cons current)
          'Do)
      (let ((swing-string
             (shm-kill-node 'buffer-substring-no-properties
                            current
                            (shm-node-start (shm-node-child current-pair)))))
        (shm/newline-indent)
        (shm-insert-indented (lambda () (insert swing-string)))))
     ((eq (shm-node-cons current)
          'Var)
      (let* ((next-pair (shm-node-next current-pair))
             (parent-pair (shm-node-parent current-pair))
             (start (shm-node-start-column (cdr parent-pair))))
        (let ((swing-string
               (shm-kill-region 'buffer-substring-no-properties
                                (shm-node-start (cdr next-pair))
                                (shm-node-end (cdr parent-pair))
                                nil)))
          (shm/reparse)
          (forward-char -1)
          (newline)
          (indent-to (+ (shm-indent-spaces)
                        start))
          (shm-insert-indented (lambda () (insert swing-string))))))
     (t
      (error "Don't know how to swing that kind of expression.")))))

(defun shm/swing-up ()
  "Swing the children of the current node upwards.

hai = do
  foo bar
  mu zot

With the cursor on `do', this will produce:

hai = do foo bar
         mu zot
"
  (interactive)
  (let* ((current-pair (shm-current-node-pair))
         (current (cdr current-pair)))
    (cond
     ((eq (shm-node-cons current)
          'Do)
      (let ((swing-string
             (shm-kill-node 'buffer-substring-no-properties
                            current
                            (shm-node-start (shm-node-child current-pair)))))
        (delete-indentation)
        (if (looking-at " ")
            (forward-char 1)
          (insert " "))
        (shm-insert-indented (lambda () (insert swing-string)))))
     (t
      (error "Don't know how to swing that kind of expression.")))))

(defun shm/newline-indent ()
  "Make a newline and indent, making sure to drag anything down, re-indented
  with it."
  (interactive)
  (cond
   ((and (shm-in-string)
         (not (= (shm-node-start (shm-current-node))
                 (point))))
    (let ((column (shm-node-start-column (shm-current-node))))
      (insert "\\")
      (newline)
      (indent-to column)
      (insert "\\")))
   ((and (looking-at "[^])}\"]") ;; This is a cheap solution. It
         ;; could use node boundaries
         ;; instead.
         (not (looking-at "$"))
         (looking-back " "))
    (shm/reparse)
    ;; If there's some stuff trailing us, then drag that with us.
    (let ((newline-string (shm-kill-node 'buffer-substring-no-properties
                                         (cdr (shm-node-ancestor-at-point (shm-current-node-pair)
                                                                          (point)))
                                         (point)))
          (point (point)))
      (shm-newline-indent t newline-string)
      (shm-insert-indented
       (lambda ()
         (insert newline-string))
       t)))
   ;; Otherwise just do the indent.
   (t (shm/reparse)
      (shm-newline-indent nil)))
  (shm/reparse))

(defun shm-newline-indent (dragging &optional newline-string)
  "Go to the next logical line from the current node at the right column.

This function uses the node's type to decode how to indent, and
in some cases will insert commas and things like for tuples and
lists.

DRAGGING indicates whether this indent will drag a node downwards."
  (let* ((current-pair (shm-current-node-pair))
         (current (cdr current-pair))
         (parent-pair (shm-node-parent current-pair))
         (parent (cdr parent-pair))
         (inhibit-read-only t))
    (cond
     ((or (string= (shm-node-type-name current)
                   "ImportSpecList")
          (and (string= (shm-node-type-name current)
                        "ModuleName")
               (looking-at "$")
               parent
               (string= (shm-node-type-name parent)
                        "ImportDecl")))
      (newline)
      (insert "import "))
     ((and (or (string= "Type" (shm-node-type-name current))
               (string= "Context" (shm-node-type-name current)))
           (eq 'TypeSig (shm-node-cons (shm-decl-node (point)))))
      (let ((column (save-excursion (search-backward-regexp " :: ")
                                    (+ 4 (current-column)))))
        (newline)
        (indent-to column)
        (when (and dragging
                   (or (string-match "^=>" newline-string)
                       (string-match "^->" newline-string)))
          (delete-region (- (point) 3) (point)))))
     ;; List comprehensions
     ((and parent
           (eq 'QualStmt (shm-node-cons parent)))
      (newline)
      (indent-to (1- (shm-node-start-column parent)))
      (insert ",")
      (shm-set-node-overlay parent-pair))
     ;; When inside a list, indent to the list's position with an
     ;; auto-inserted comma.
     ((and parent
           (or (eq 'List (shm-node-cons parent))
               (eq 'Tuple (shm-node-cons parent))
               (eq 'QualStmt (shm-node-cons parent))))
      (let* ((first-item-on-line (save-excursion
                                   (goto-char (shm-node-start current))
                                   (search-backward-regexp "[[,][ ]*")
                                   (= (current-column)
                                      (shm-node-start-column parent)))))
        (newline)
        (indent-to (shm-node-start-column parent))
        (insert ",")
        (when first-item-on-line
          (insert (make-string (- (shm-node-start-column current)
                                  (current-column))
                               ? )))
        (shm-set-node-overlay parent-pair)))
     ;; Lambdas indents k spaces inwards
     ((eq 'Lambda (shm-node-cons current))
      (newline)
      (indent-to (+ (shm-indent-spaces) (shm-node-start-column current))))
     ;; Indentation for RHS
     ((and parent
           (eq 'App (shm-node-cons parent))
           (= (shm-node-start current)
              (shm-node-start parent)))
      (let ((ancestor-parent (shm-node-parent
                              (shm-node-ancestor-at-point current-pair (point))))
            (decl (shm-node-parent current-pair "Decl SrcSpanInfo")))
        (newline)
        (indent-to (+ (shm-indent-spaces)
                      (shm-node-start-column (cdr decl))))))
     ;; Indentation for function application.
     ((and parent
           (or (eq 'App (shm-node-cons parent))
               (eq 'TyApp (shm-node-cons parent))))
      (let ((column
             (save-excursion
               (if (/= (shm-node-start-line current)
                      (shm-node-start-line parent))
                   (shm-node-start-column current)
                 (progn (shm/goto-parent)
                        (forward-sexp)
                        (1+ (current-column))))))
            (previous
             (when (looking-back " ")
               (save-excursion
                 (search-backward-regexp "[ ]+"
                                         (line-beginning-position)
                                         t
                                         1)
                 (let ((prev (shm-current-workable-node)))
                   (when (and (= (car (shm-node-parent prev))
                                 (car parent-pair))
                              (/= (shm-node-start parent)
                                  (shm-node-start (cdr prev))))
                     prev))))))
        (cond
         (previous
          (newline)
          (indent-to (shm-node-start-column (cdr previous))))
         ((and (or (= column (current-column))
                   (= column (+ (shm-node-start-column parent)
                                (shm-indent-spaces))))
               (/= column (shm-node-start-column parent)))
          (newline)
          (indent-to (+ (shm-node-start-column parent)
                        (shm-indent-spaces))))
         (t
          (newline)
          (indent-to column)))))
     ;; Indent for sum types
     ((or (and parent
               (eq 'DataDecl (shm-node-cons parent)))
          (eq 'ConDecl (shm-node-cons current)))
      (newline)
      (indent-to (shm-node-start-column current))
      (delete-char -2)
      (insert "| "))
     ;; Auto-insert commas for field updates
     ((or (string= "FieldUpdate" (shm-node-type-name current))
          (string= "FieldDecl" (shm-node-type-name current))
          (string= "ExportSpec" (shm-node-type-name current))
          (string= "ImportSpec" (shm-node-type-name current)))
      ;; This is hacky because HSE doesn't have special nodes for the
      ;; record and the update in record {update} and so we have to
      ;; figure out where the { starts. There is some additional
      ;; information in HSE's trees, but I haven't thought of a nice
      ;; way to extract that yet.
      (goto-char (shm-node-end parent))
      (backward-sexp)
      (let ((column (current-column)))
        (goto-char (shm-node-end current))
        (newline)
        (indent-to column)
        (insert ",")
        (insert (make-string (abs (- (shm-node-start-column current)
                                     (1+ column)))
                             ? ))
        (shm/init)))
     ((and parent
           (eq 'Lambda (shm-node-cons parent)))
      (cond
       ((eq shm-lambda-indent-style 'leftmost-parent)
        (let ((leftmost-parent (cdr (shm-find-furthest-parent-on-line parent-pair))))
          (newline)
          (indent-to (+ (shm-indent-spaces)
                        (shm-node-indent-column leftmost-parent)))))
       (t (newline)
          (indent-to (+ (shm-indent-spaces)
                        (shm-node-start-column parent))))))
     ;; Guards | foo = …
     ((or (string= "GuardedRhs" (shm-node-type-name current))
          (string= "GuardedAlt" (shm-node-type-name current)))
      (newline)
      (indent-to (shm-node-start-column current))
      (insert "| "))
     ;; Indent after or at the = (an rhs).
     ((and parent
           (or (string= "Rhs" (shm-node-type-name parent))
               (string= "Rhs" (shm-node-type-name current))
               (string= "GuardedAlt" (shm-node-type-name parent))
               (string= "GuardedRhs" (shm-node-type-name parent))))
      (newline)
      (indent-to (+ (shm-indent-spaces)
                    (shm-node-start-column (cdr (shm-node-parent parent-pair))))))
     ;; When in a field update.
     ((and parent
           (string= "FieldUpdate" (shm-node-type-name parent)))
      (newline)
      (indent-to (+ (shm-node-start-column parent)
                    (shm-indent-spaces))))
     ;; When in an alt list
     ((and parent
           (string= "GuardedAlts" (shm-node-type-name current)))
      (newline)
      (indent-to (+ (shm-node-start-column parent)
                    (shm-indent-spaces))))
     ;; When in a case alt.
     ((and parent
           (string= "GuardedAlts" (shm-node-type-name parent)))
      (newline)
      (let ((alt (cdr (shm-node-parent parent-pair))))
        (indent-to (+ (shm-node-start-column alt)
                      (shm-indent-spaces)))))
     ;; Copy infix operators similar to making new list/tuple
     ;; separators
     ((and parent
           (eq 'InfixApp (shm-node-cons parent)))
      (let* ((operand-pair (shm-node-previous current-pair))
             (operand (cdr operand-pair))
             (string (buffer-substring-no-properties (shm-node-start operand)
                                                     (shm-node-end operand))))
        (cond
         (dragging
          (newline)
          (indent-to (shm-node-start-column parent)))
         ((save-excursion (goto-char (shm-node-end operand))
                          (= (point) (line-end-position)))
          (insert " " string)
          (newline)
          (indent-to (shm-node-start-column current)))
         (t
          (newline)
          (indent-to (shm-node-start-column operand))
          (insert string " ")))))
     ;; Infix operators
     ((and parent
           (eq 'InfixApp (shm-node-cons parent)))
      (newline)
      (indent-to (+ (shm-node-start-column parent))))
     ;; ((eq 'Alt (shm-node-cons current))
     ;;  (newline)
     ;;  (indent-to (shm-node-start-column current))
     ;;  (when shm-auto-insert-skeletons
     ;;    (save-excursion (insert "_ -> undefined"))
     ;;    (shm-evaporate (point) (+ (point) 1))
     ;;    (shm-evaporate (+ (point) (length "_ -> "))
     ;;                   (+ (point) (length "_ -> undefined")))))
     ;; Commenting out this behaviour for now
     ;; ((string= "Match" (shm-node-type-name current))
     ;;  (let ((name (cdr (shm-node-child-pair current-pair))))
     ;;    (newline)
     ;;    (indent-to (shm-node-start-column current))
     ;;    (insert (buffer-substring-no-properties (shm-node-start name)
     ;;                                            (shm-node-end name))
     ;;            " ")))
     ;; Default indentation just copies the current node's indentation
     ;; level. Generally works reliably, but has less than favourable
     ;; indentation sometimes. It just serves as a catch-all.
     (t
      (newline)
      (indent-to (shm-node-start-column current))))))

(provide 'shm-indent)
