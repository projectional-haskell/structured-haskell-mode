;;; shm-layout.el --- Layout-sensitive tasks

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

(require 'shm-node)
(require 'shm-ast)

(defun shm-appropriate-adjustment-point (direction)
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
  (unless (eolp)
    (let ((current (shm-current-node)))
      (case direction
        ('forward
         (when (and current
                    (< (shm-node-end current) (line-end-position))
                    (not (and (looking-at " ")
                              (looking-back " "))))
           (goto-char (shm-node-end current))))
        ('backward
         (when (and current
                    (> (shm-node-start current) (line-beginning-position)))
           (goto-char (shm-node-start current))))))))

(defun shm-adjust-dependents (end-point n)
  "Adjust dependent lines by N characters that depend on this
line after END-POINT."
  (unless (= (line-beginning-position)
             (1- (point)))
    (let ((line (line-number-at-pos))
          (column (current-column)))
      (when (and (not (< column (shm-indent-spaces)))
                 ;; I don't remember what this is for. I'm removing
                 ;; it. If it causes problems, I'll deal with it then.
                 ;;
                 ;; (not (and (looking-back "^[ ]+")
                 ;;           (looking-at "[ ]*")))
                 (save-excursion (goto-char end-point)
                                 (forward-word)
                                 (= (line-number-at-pos) line)))
        (unless (save-excursion
                  (goto-char (line-end-position))
                  (let ((current-pair (shm-node-backwards)))
                    (when current-pair
                      (or (string= (shm-node-type-name (cdr current-pair))
                                   "Rhs")
                          (eq (shm-node-cons (cdr current-pair))
                              'Lambda)))))
          (shm-move-dependents n
                               end-point))))))

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
    (shm-appropriate-adjustment-point 'forward)
    (shm-adjust-dependents (point) (length string)))
  (insert string)
  (shm/init t))

(defun shm-insert-indented (do-insert &optional no-adjust-dependents)
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
  (let* ((column (current-column))
         (line (line-beginning-position))
         (start (point))
         (string
          (with-temp-buffer
            (funcall do-insert)
            (buffer-string)))
         (swinging
          (with-temp-buffer
            (insert string)
            (get-text-property (point-min) 'shm-swinging-expr)))
         (current-node-pair
          (when swinging
            (shm-current-node-pair)))
         (furthest-parent
          (when current-node-pair
            (shm-find-furthest-parent-on-line current-node-pair))))
    (insert (if (shm-in-string)
                (replace-regexp-in-string
                 "\n" "\\\\n\\\\\n\\\\"
                 string)
              string))
    (when (and (= line (line-beginning-position))
               (not no-adjust-dependents))
      (shm-adjust-dependents start (- (current-column)
                                      column)))
    (when (= (point) start)
      (goto-char (region-end)))
    (let ((end (point)))
      (cond
       (swinging
        (when furthest-parent
          (let ((node (cdr furthest-parent)))
            (goto-char end)
            (indent-rigidly start
                            end
                            (+ (shm-indent-spaces)
                               (shm-node-indent-column node))))))
       (t (goto-char end)
          (indent-rigidly start end column)))
      (push-mark)
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
              (when multi-line
                (insert (make-string start-col ? )))
              (insert string)
              ;; This code de-indents code until a single line is
              ;; hitting column zero.
              (let ((indent-tabs-mode nil)
                    (continue t)
                    (buffer-max (point-max)))
                (while (and continue
                            (progn (goto-char (point-min))
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
                                                      nil))))))

                  ;; Bring everything back one.
                  (unless (= 0 start-col)
                    (indent-rigidly (point-min) (point-max)
                                    -1))
                  (if (/= buffer-max (point-max))
                      (setq buffer-max (point-max))
                    (setq continue nil))))
              ;; If there's an empty line at the end, then strip that
              ;; out. It's just bothersome when pasting back in.
              (goto-char (point-max))
              (when (looking-at "^$")
                (delete-region (1- (point))
                               (point)))
              (when (> (count-lines (point-min)
                                    (point-max))
                       1)
                (let* ((first-line-col (save-excursion (goto-char (point-min))
                                                       (back-to-indentation)
                                                       (current-column)))
                       (second-line-col (save-excursion (goto-char (point-min))
                                                        (forward-line)
                                                        (back-to-indentation)
                                                        (current-column))))
                  (when (> first-line-col second-line-col)
                    (goto-char (point-min))
                    (indent-rigidly (line-beginning-position)
                                    (line-end-position)
                                    (- first-line-col))
                    (put-text-property (point-min)
                                       (point-max)
                                       'shm-swinging-expr
                                       t))))
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
         (parent-pair (shm-node-ancestor-for-kill current-pair (point)))
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

(defun shm-node-ancestor-for-kill (current-pair point)
  "Get the ancestor for greedy killing."
  (let* ((current (cdr current-pair))
         (parent-pair (shm-node-parent current-pair))
         (parent (cdr parent-pair)))
    (if (and (shm-node-app-p parent)
             (< (shm-node-end current) (line-end-position)))
        parent-pair
      (shm-node-ancestor-at-point current-pair point))))

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

(provide 'shm-layout)
