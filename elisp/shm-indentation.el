;;; shm-indentation.el --- Indentation functions

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

(require 'shm-nodes)
(require 'shm-navigation)

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
    ;; If there's some stuff trailing us, then drag that with us.
    (let ((newline-string (shm-kill-node 'buffer-substring-no-properties))
          (point (point)))
      (shm-newline-indent t)
      (shm-insert-indented
       (lambda ()
         (insert newline-string)))))
   ;; Otherwise just do the indent.
   (t (shm-newline-indent nil))))

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
                           (newline))
                         (indent-to
                          (+ 2
                             (shm-node-start-column
                              (cdr (shm-node-parent (cons i rhs))))))
                         (insert "where ")))))))))

(defun shm/tab ()
  "Either indent if at the start of a line, or jump to the next
  slot."
  (interactive)
  (cond
   ((save-excursion (goto-char (line-beginning-position))
                    (looking-at "^[ ]*$"))
    (shm/simple-indent))
   (t
    (shm/jump-to-slot))))

(defun shm/backtab ()
  "Either de-indent if at the start of a line, or jump to the previous
  slot."
  (interactive)
  (cond
   ((save-excursion (goto-char (line-beginning-position))
                    (looking-at "^[ ]*$"))
    (shm/simple-indent-backtab))
   (t
    (shm/jump-to-previous-slot))))

(defun shm/simple-indent ()
  "Space out to under next visible indent point.
Indent points are positions of non-whitespace following whitespace in
lines preceeding point.  A position is visible if it is to the left of
the first non-whitespace of every nonblank line between the position and
the current line.  If there is no visible indent point beyond the current
column, `tab-to-tab-stop' is done instead."
  (interactive)
  (let* ((start-column (current-column))
         (invisible-from nil)           ; `nil' means infinity here
         (indent
          (catch 'shm-simple-indent-break
            (save-excursion
              (while (progn (beginning-of-line)
                            (not (bobp)))
                (forward-line -1)
                (if (not (looking-at "[ \t]*\n"))
                    (let ((this-indentation (current-indentation)))
                      (if (or (not invisible-from)
                              (< this-indentation invisible-from))
                          (if (> this-indentation start-column)
                              (setq invisible-from this-indentation)
                            (let ((end (line-beginning-position 2)))
                              (move-to-column start-column)
                              ;; Is start-column inside a tab on this line?
                              (if (> (current-column) start-column)
                                  (backward-char 1))
                              (or (looking-at "[ \t]")
                                  (skip-chars-forward "^ \t" end))
                              (skip-chars-forward " \t" end)
                              (let ((col (current-column)))
                                (throw 'shm-simple-indent-break
                                       (if (or (= (point) end)
                                               (and invisible-from
                                                    (> col invisible-from)))
                                           invisible-from
                                         col)))))))))))))
    (if indent
        (let ((opoint (point-marker)))
          (indent-line-to indent)
          (if (> opoint (point))
              (goto-char opoint))
          (set-marker opoint nil))
      (tab-to-tab-stop))))

(defun shm/simple-indent-backtab ()
  "Indent backwards. Dual to `shm-simple-indent'."
  (interactive)
  (let ((current-point (point))
        (i 0)
        (x 0))
    (goto-char (line-beginning-position))
    (save-excursion
      (while (< (point) current-point)
        (shm/simple-indent)
        (setq i (+ i 1))))
    (while (< x (- i 1))
      (shm/simple-indent)
      (setq x (+ x 1)))))

(defun shm/simple-indent-newline-same-col ()
  "Make a newline and go to the same column as the current line."
  (interactive)
  (let ((point (point)))
    (let ((start-end
           (save-excursion
             (let* ((start (line-beginning-position))
                    (end (progn (goto-char start)
                                (search-forward-regexp
                                 "[^ ]" (line-end-position) t 1))))
               (when end (cons start (1- end)))))))
      (if start-end
          (progn (newline)
                 (insert (buffer-substring-no-properties
                          (car start-end) (cdr start-end))))
        (newline)))))

(defun shm/simple-indent-newline-indent ()
  "Make a newline on the current column and indent on step."
  (interactive)
  (shm/simple-indent-newline-same-col)
  (insert (make-string (shm-indent-spaces) ? )))

(defun shm-appropriate-adjustment-point ()
  "Go to the appropriate adjustment point.

This is called before calling `shm-adjust-dependents', because some places, e.g.

zoo = do
  bar
  mu

If the point is at 'z', then we should *not* move 'bar' or 'mu',
even though we normally would. To avoid doing this, we use a very
simple but 90% effective (100% is rather hard, will not be
appearing in a beta version) heuristic. We jump to here:

zoo| = do
  bar
  mu

And use our normal adjustment test there. After all, only thing
after 'zoo' are *really* dependent."
  (let ((current (shm-current-node)))
    (when (and current
               (<= (shm-node-end current) (line-end-position)))
      (goto-char (shm-node-end current)))))

(defun shm-newline-indent (dragging)
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
     ;; When inside a list, indent to the list's position with an
     ;; auto-inserted comma.
     ((and parent
           (eq 'List (shm-node-cons parent)))
      (newline)
      (indent-to (shm-node-start-column parent))
      (insert ",")
      (shm-set-node-overlay parent-pair))
     ;; Lambdas indents k spaces inwards
     ((eq 'Lambda (shm-node-cons current))
      (newline)
      (indent-to (+ (shm-indent-spaces) (shm-node-start-column current))))
     ;; Indentation for function application.
     ((and parent
           (or (eq 'App (shm-node-cons parent))
               (eq 'TyApp (shm-node-cons parent))))
      (let ((column
             (save-excursion
               (shm/goto-parent)
               (forward-sexp)
               (1+ (current-column)))))
        (cond

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

(defun shm-adjust-dependents (end-point n)
  "Adjust dependent lines by N characters that depend on this
line after END-POINT."
  (let ((line (line-number-at-pos)))
    (when (and (not (and (looking-back "^[ ]+")
                         (looking-at "[ ]*")))
               (save-excursion (goto-char end-point)
                               (forward-word)
                               (= (line-number-at-pos) line)))
      (shm-move-dependents n
                           end-point))))

(defun shm-move-dependents (n point)
  "Move dependent-with-respect-to POINT lines N characters forwards or backwards.

This is purely based on alignments. If anything is aligned after
the current column, then it's assumed to be a child of whatever
has recently changed at POINT, and thus we 'bring it along'
either forwards or backwards.

The algorithm isn't quite comprehensive, it needs special cases
for top-level functions and things like that."
  (save-excursion
    (let ((column (progn (goto-char point)
                         (current-column)))
          (point nil)
          (end-point nil))
      (while (and (= 0 (forward-line 1))
                  (or (not end-point)
                      (/= end-point (line-end-position))))
        (if (shm-line-indented-past (1+ column))
            (progn (unless point
                     (setq point (goto-char (line-beginning-position))))
                   (setq end-point (line-end-position)))
          (goto-char (point-max))))
      (when end-point
        (indent-rigidly point end-point n)))))

(defun shm-line-indented-past (n)
  "Is the current line indented past N?"
  (goto-char (line-beginning-position))
  (let ((column (search-forward-regexp "[^ ]" (line-end-position) t 1)))
    (if column
        (>= (1- (current-column)) n)
      t)))

(defun shm-insert-string (string)
  "Insert the given string."
  (save-excursion
    (shm-appropriate-adjustment-point)
    (shm-adjust-dependents (point) (length string)))
  (insert string)
  (shm/init t))

(defun shm-insert-indented (do-insert)
  "Insert, indented in The Right Way. Calls DO-INSERT to do the insertion.

This function assumes a certain semantic meaning towards the
contents of the kill ring. That is,

do bar
   mu

Is an expression which, when pasted, into

main =

should yield,

main = do bar
          mu

Which is so convenient it changes the way you work. However,
there is also the other case:

  do
bar
mu

This is what happens when you have expressions whose children
hang on the underside, and thus pasting these can be done in two
ways: (1) the above way, (2) or like this:

main = do
  bar
  mu

I.e. take the parent into account and try to re-paste an
underside dangling expression. I don't like this style. With SHM
this style becomes pointless and in fact detrimental. It's much
easier to read and manipulate children who are next to their
parents. But one must compromise and conform to some styles no
matter how poorly reasoned.

We can actually give the option for people to pick and choose
this underside dangling vs not. But that will be implemented as a
separate function rather than hard-coded into this one specific
operation."
  (let ((column (current-column))
        (line (line-beginning-position))
        (start (point))
        (in-string (shm-in-string)))
    (let ((string (with-temp-buffer
                    (funcall do-insert)
                    (buffer-string))))
      (insert
       ;; Pasting inside a string should escape double quotes and
       ;; convert newlines to multiline strings.
       (if in-string
           (replace-regexp-in-string
            "\"" "\\\\\""
            (replace-regexp-in-string
             "\n" "\\\\n\\\\\n\\\\"
             string))
         string)))
    (when (= line (line-beginning-position))
      (shm-adjust-dependents start (- (current-column)
                                      column)))
    (let ((end (point)))
      (cond
       ((progn (goto-char start)
               (looking-at " "))
        (let ((node (cdr (shm-find-furthest-parent-on-line (shm-current-node-pair)))))
          (goto-char end)
          (indent-rigidly start
                          end
                          (+ (shm-indent-spaces)
                             (shm-node-indent-column node)))
          (delete-region start
                         (save-excursion
                           (goto-char start)
                           (search-forward-regexp "[ ]+" (line-end-position) t 1)))))
       (t (goto-char end)
          (indent-rigidly start end
                          (if in-string
                              (1- column)
                            column))))
      (setq shm-last-yanked (list start (point)))
      (goto-char start))))

(defun shm-find-furthest-parent-on-line (current)
  "Find the parent which starts nearest to column 0 on the
current line.

This is used when indenting dangling expressions."
  (let ((parent (shm-node-parent current)))
    (if parent
        (if (= (line-beginning-position)
               (save-excursion (goto-char (shm-node-start (cdr parent)))
                               (line-beginning-position)))
            (shm-find-furthest-parent-on-line parent)
          current)
      current)))

(defun shm-indent-spaces ()
  "Get the number of spaces to indent."
  (if (boundp 'haskell-indent-spaces)
      haskell-indent-spaces
    shm-indent-spaces))

(provide 'shm-indentation)
