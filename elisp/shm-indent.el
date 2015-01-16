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
(require 'shm-simple-indent)

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
       (t
        (let ((current (shm-current-node)))
          (let ((old-column (current-column)))
            (delete-region (line-beginning-position) (point))
            (delete-char -1)
            (let ((new-column (current-column)))
              (indent-rigidly (line-end-position)
                              (shm-node-end current)
                              (abs (- old-column new-column))))))
        (when nil
          (let ((string (shm-kill-node 'buffer-substring-no-properties)))
            (delete-indentation)
            (insert " ")
            (shm-insert-indented
             (lambda ()
               (insert string)))))))
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
      (save-excursion
        (let ((new-column (shm-get-swing-column current)))
          (goto-char (shm-node-start current))
          (forward-word 1)
          (search-forward " ")
          (let ((old-column (current-column)))
            (insert "\n")
            (indent-rigidly (point)
                            (shm-node-end current)
                            (- old-column))
            (indent-rigidly (point)
                            (shm-node-end current)
                            new-column)))
        (shm/reparse)))
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
          (shm-newline)
          (indent-to (+ (shm-indent-spaces)
                        start))
          (shm-insert-indented (lambda () (insert swing-string))))))
     (t
      (error "Don't know how to swing that kind of expression.")))))

(defun shm-get-swing-column (node)
  "Get the column that a node would be newline-indented to."
  (save-excursion
    (let ((start (shm-node-start node)))
      (goto-char start)
      (shm-newline-indent nil nil)
      (let ((column (current-column)))
        (delete-region start (point))
        column))))

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
   ((or (= (line-beginning-position)
           (line-end-position))
        (not (shm-current-node)))
    (if (= (line-beginning-position)
           (line-end-position))
        (newline)
      (progn (newline)
             (shm/simple-indent))))
   ((and (shm-in-string)
         (not (= (shm-node-start (shm-current-node))
                 (point))))
    (let ((column (shm-node-start-column (shm-current-node))))
      (insert "\\")
      (shm-newline)
      (indent-to column)
      (insert "\\")))
   ((and (looking-at "[^])}\"]") ;; This is a cheap solution. It
         ;; could use node boundaries
         ;; instead.
         (not (looking-at "$"))
         (looking-back " "))
    (shm/reparse)
    (let ((newline-string (buffer-substring-no-properties (point)
                                                          (shm-node-end (shm-current-node))))
          ;; This is like (line-end-position), but if the line ends in
          ;; a closing delimiter like ), then *really* the "end" of
          ;; the thing we're dragging should be inside these
          ;; delimiters.
          (end-position (save-excursion
                          (goto-char (line-end-position))
                          (when (looking-back "[])}\"]+")
                            (search-backward-regexp "[^])}\"]")
                            (forward-char 1))
                          (point))))
      ;; If we're going to drag something, that means the *real* parent
      ;; should encompass whatever we're going to drag, and that should
      ;; be at or beyond the end of the line.
      (unless (looking-at "\\(=>\\|->\\)")
        (let ((current (shm-current-node-pair)))
          (while (and (not (>= (shm-node-end (cdr current))
                               end-position))
                      (/= (car current)
                          (car (shm-node-ancestor-at-point current
                                                           (shm-node-start (cdr current))))))
            (shm/goto-parent)
            (setq current (shm-current-node-pair)))))
      ;; If there's some stuff trailing us, then drag that with us.
      (let* ((current (shm-current-node))
             (old-column (shm-node-start-column current)))
        (shm-newline-indent t
                            newline-string)
        (let ((new-column (current-column)))
          (indent-rigidly (point)
                          (shm-node-end current)
                          (- (abs (- old-column new-column))))))))
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
      (shm-newline)
      (insert "import "))
     ((and (or (string= "Type" (shm-node-type-name current))
               (string= "Context" (shm-node-type-name current)))
           (eq 'TypeSig (shm-node-cons (shm-decl-node (point)))))
      (let ((column (save-excursion (search-backward-regexp " :: ")
                                    (+ 4 (current-column)))))
        (shm-newline)
        (indent-to column)
        (when (and dragging
                   (or (string-match "^=>" newline-string)
                       (string-match "^->" newline-string)))
          (delete-region (- (point) 3) (point)))))
     ;; List comprehensions
     ((and parent
           (eq 'QualStmt (shm-node-cons parent)))
      (shm-newline)
      (indent-to (1- (shm-node-start-column parent)))
      (insert ",")
      (shm-set-node-overlay parent-pair))
     ;; When inside a list, indent to the list's position with an
     ;; auto-inserted comma.
     ((and parent
           (or (eq 'List (shm-node-cons parent))
               (eq 'Tuple (shm-node-cons parent))
               (eq 'QualStmt (shm-node-cons parent))))
      (shm-newline-indent-listish current parent parent-pair))
     ;; Lambdas indents k spaces inwards
     ((eq 'Lambda (shm-node-cons current))
      (shm-newline)
      (indent-to (+ (shm-indent-spaces) (shm-node-start-column current))))
     ;; Indentation for RHS
     ((and parent
           (eq 'App (shm-node-cons parent))
           (= (shm-node-start current)
              (shm-node-start parent)))
      (let ((ancestor-parent (shm-node-parent
                              (shm-node-ancestor-at-point current-pair (point))))
            (decl (shm-node-parent current-pair "Decl SrcSpanInfo")))
        (shm-newline)
        (indent-to (+ (shm-indent-spaces)
                      (shm-node-start-column (cdr decl))))))
     ;; Indentation for function application.
     ((and parent
           (or (eq 'App (shm-node-cons parent))
               (eq 'TyApp (shm-node-cons parent))
               (eq 'ConDecl (shm-node-cons parent))))
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
          (shm-newline)
          (indent-to (shm-node-start-column (cdr previous))))
         ((and (or (= column (current-column))
                   (= column (+ (shm-node-start-column parent)
                                (shm-indent-spaces))))
               (/= column (shm-node-start-column parent)))
          (shm-newline)
          (indent-to (+ (shm-node-start-column parent)
                        (shm-indent-spaces))))
         (t
          (shm-newline)
          (indent-to column)))))
     ;; Indent for sum types
     ((or (and parent
               (eq 'DataDecl (shm-node-cons parent)))
          (eq 'ConDecl (shm-node-cons current)))
      (shm-newline)
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
        (shm-newline)
        (indent-to column)
        (insert ",")
        (insert (make-string (abs (- (shm-node-start-column current)
                                     (1+ column)))
                             ? ))
        (shm-auto-insert-field-prefix current parent)
        (shm/init)))
     ((and parent
           (eq 'Lambda (shm-node-cons parent)))
      (cond
       ((eq shm-lambda-indent-style 'leftmost-parent)
        (let ((leftmost-parent (cdr (shm-find-furthest-parent-on-line parent-pair t))))
          (shm-newline)
          (indent-to (+ (shm-indent-spaces)
                        (shm-node-indent-column leftmost-parent)))))
       (t (shm-newline)
          (indent-to (+ (shm-indent-spaces)
                        (shm-node-start-column parent))))))
     ;; Guards | foo = …
     ((or (string= "GuardedRhs" (shm-node-type-name current))
          (string= "GuardedAlt" (shm-node-type-name current)))
      (shm-newline)
      (indent-to (shm-node-start-column current))
      (insert "| "))
     ;; Indent after or at the = (an rhs).
     ((and parent
           (or (string= "Rhs" (shm-node-type-name parent))
               (string= "Rhs" (shm-node-type-name current))
               (string= "GuardedAlt" (shm-node-type-name parent))
               (string= "GuardedRhs" (shm-node-type-name parent))))
      (shm-newline)
      (indent-to (+ (shm-indent-spaces)
                    (shm-node-start-column (cdr (shm-node-parent parent-pair))))))
     ;; When in a field update.
     ((and parent
           (string= "FieldUpdate" (shm-node-type-name parent)))
      (shm-newline)
      (indent-to (+ (shm-node-start-column parent)
                    (shm-indent-spaces))))
     ;; When in an alt list
     ((and parent
           (string= "GuardedAlts" (shm-node-type-name current)))
      (shm-newline)
      (indent-to (+ (shm-node-start-column parent)
                    (shm-indent-spaces))))
     ;; When in a case alt.
     ((and parent
           (string= "GuardedAlts" (shm-node-type-name parent)))
      (shm-newline)
      (let ((alt (cdr (shm-node-parent parent-pair))))
        (indent-to (+ (shm-node-start-column alt)
                      (shm-indent-spaces)))))
     ;; Infix operators
     ((and parent
           (eq 'InfixApp (shm-node-cons parent)))
      (shm-newline)
      (indent-to (+ (shm-node-start-column parent))))
     ((and parent
           (eq 'Paren (shm-node-cons parent)))
      (shm-newline-indent-listish current parent parent-pair))
     ;; Default indentation just copies the current node's indentation
     ;; level. Generally works reliably, but has less than favourable
     ;; indentation sometimes. It just serves as a catch-all.
     (t
      (shm-newline)
      (indent-to (shm-node-start-column current))))))

(defun shm-newline-indent-listish (current parent parent-pair)
  "Indent and insert a comma for a list-ish syntactical node."
  (let* ((first-item-on-line (and (not (looking-at ","))
                                  (save-excursion
                                    (goto-char (shm-node-start current))
                                    (search-backward-regexp "[[,][ ]*")
                                    (= (current-column)
                                       (shm-node-start-column parent)))))
         (go-back (and first-item-on-line
                       (= (point) (shm-node-start current))))
         (already-have-comma (looking-back ",")))
    (shm-newline)
    (indent-to (shm-node-start-column parent))
    ;; Don't insert duplicate commas.
    (unless (or (looking-at ",") already-have-comma)
      (insert ","))
    (when go-back
      (let ((column (current-column)))
        (forward-line -1)
        (forward-char column)))
    (when first-item-on-line
      (insert (make-string (- (shm-node-start-column current)
                              (current-column))
                           ? )))
    (unless (or (looking-back ",")
                (looking-at ","))
      (insert " "))
    (shm-set-node-overlay parent-pair)))

;; Copy infix operators similar to making new list/tuple
;; separators
;; ((and parent
;;       (eq 'InfixApp (shm-node-cons parent)))
;;  (let* ((operand-pair (shm-node-previous current-pair))
;;         (operand (cdr operand-pair))
;;         (string (buffer-substring-no-properties (shm-node-start operand)
;;                                                 (shm-node-end operand))))
;;    (cond
;;     (dragging
;;      (shm-newline)
;;      (indent-to (shm-node-start-column parent)))
;;     ((save-excursion (goto-char (shm-node-end operand))
;;                      (= (point) (line-end-position)))
;;      (insert " " string)
;;      (shm-newline)
;;      (indent-to (shm-node-start-column current)))
;;     (t
;;      (shm-newline)
;;      (indent-to (shm-node-start-column operand))
;;      (insert string " ")))))

;; A case for shm-newline-indent which will copy a case-alt. Not
;; determined how to best include this feature yet.
;;
;; ((eq 'Alt (shm-node-cons current))
;;  (shm-newline)
;;  (indent-to (shm-node-start-column current))
;;  (when shm-auto-insert-skeletons
;;    (save-excursion (insert "_ -> undefined"))
;;    (shm-evaporate (point) (+ (point) 1))
;;    (shm-evaporate (+ (point) (length "_ -> "))
;;                   (+ (point) (length "_ -> undefined")))))
;; Commenting out this behaviour for now
;; ((string= "Match" (shm-node-type-name current))
;;  (let ((name (cdr (shm-node-child-pair current-pair))))
;;    (shm-newline)
;;    (indent-to (shm-node-start-column current))
;;    (insert (buffer-substring-no-properties (shm-node-start name)
;;                                            (shm-node-end name))
;;            " ")))

(defun shm-auto-insert-field-prefix (current parent)
  "Auto insert prefixes of fields in record declarations. Example:

data Person = Person
  { personAge :: Int
  , person|

"
  (when (string= "FieldDecl" (shm-node-type-name current))
    (let* ((cur-substr
            (save-excursion
              (goto-char (shm-node-start current))
              (buffer-substring-no-properties (point)
                                              (progn (forward-word 1)
                                                     (point)))))
           (type-name
            (save-excursion
              (goto-char (shm-node-start parent))
              (buffer-substring-no-properties (point)
                                              (progn (forward-word 1)
                                                     (point)))))
           (prefix
            (if (string-match "\\([A-Z]\\)\\(.*\\)"
                              type-name)
                (concat (downcase (match-string 1 type-name))
                        (match-string 2 type-name))
              type-name)))
      (when (string-prefix-p prefix cur-substr)
        (insert prefix)))))

(defun shm-newline ()
  "Normal `newline' does funny business. What we want is to
literally insert a newline and no more."
  (insert "\n"))

(defun shm/split-line ()
  "Split line."
  (interactive)
  (if (shm-literal-insertion)
      (call-interactively 'split-line)
    (save-excursion
      (let ((column (current-column)))
        (insert "\n")
        (indent-to column)))))

(provide 'shm-indent)
