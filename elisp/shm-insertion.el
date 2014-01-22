;;; shm-insertion.el --- Operations to insert things

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

(require 'shm-evaporate)
(require 'shm-skeletons)
(require 'shm-nodes)

(defun shm/insert-undefined ()
  "Insert undefined."
  (interactive)
  (save-excursion
    (let ((point (point)))
      (shm-insert-string "undefined")
      (shm-evaporate point (point)))))

(defun shm/wrap-parens ()
  "Wrap the node in parentheses."
  (interactive)
  (cond
   ((region-active-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (goto-char beg)
      (insert "(")
      (goto-char (1+ end))
      (insert ")")
      (goto-char (1+ beg))))
   (t (let ((line (line-number-at-pos))
            (node (shm-current-node)))
        (save-excursion
          (goto-char (shm-node-start node))
          (insert "(")
          (goto-char (shm-node-end node))
          (when (/= line (line-number-at-pos))
            (indent-rigidly (shm-node-start node)
                            (shm-node-end node)
                            1))
          (insert ")"))
        (forward-char 1)))))

(defun shm/space ()
  "Insert a space but sometimes do something more clever, like
  inserting skeletons."
  (interactive)
  (if (and (bound-and-true-p god-local-mode)
           (fboundp 'god-mode-self-insert))
      (god-mode-self-insert)
    (cond
     ((shm-in-comment)
      (insert " "))
     (shm-auto-insert-skeletons
      (cond
       ((and (looking-back "[^a-zA-Z0-9_]do")
             (or (eolp)
                 (looking-at "[])}]")))
        (shm-auto-insert-do))
       ((and (looking-back " <-")
             (let ((current (shm-current-node)))
               (when current
                 (or (eq 'Do (shm-node-cons current))
                     (string= "Stmt" (shm-node-type-name current))))))
        (shm-auto-insert-stmt 'qualifier))
       ((looking-back "[^a-zA-Z0-9_]case")
        (shm-auto-insert-case))
       ((looking-back "[^a-zA-Z0-9_]if")
        (shm-auto-insert-if))
       ((looking-back "[^a-zA-Z0-9_]let")
        (cond
         ((let ((current (shm-current-node)))
            (not (or (eq 'Do (shm-node-cons current))
                     (eq 'BDecls (shm-node-cons current))
                     (string= "Stmt" (shm-node-type-name current)))))
          (shm-auto-insert-let))
         (t (shm-auto-insert-stmt 'let))))
       ((and (looking-back "module")
             (= (line-beginning-position)
                (- (point) 6))
             (looking-at "[ ]*$"))
        (shm-auto-insert-module))
       (t (insert " "))))
     (t (insert " ")))))

(defun shm/jump-to-slot ()
  "Jump to the next skeleton slot."
  (interactive)
  (let ((os (sort (remove-if-not (lambda (o) (overlay-get o 'shm-evaporate-overlay))
                                 (overlays-in (point) (point-max)))
                  (lambda (a b)
                    (< (overlay-start a)
                       (overlay-start b))))))
    (when os
      (if (= (overlay-start (car os))
             (point))
          (when (cadr os)
            (goto-char (overlay-start (cadr os))))
        (goto-char (overlay-start (car os)))))))

(defun shm/jump-to-previous-slot ()
  "Jump to the previous skeleton slot."
  (interactive)
  (let ((os (sort (remove-if-not (lambda (o) (overlay-get o 'shm-evaporate-overlay))
                                 (overlays-in (point-min) (point)))
                  (lambda (a b)
                    (> (overlay-start a)
                       (overlay-start b))))))
    (when os
      (if (= (overlay-start (car os))
             (point))
          (when (cadr os)
            (goto-char (overlay-start (cadr os))))
        (goto-char (overlay-start (car os)))))))

(defun shm/double-quote ()
  "Insert double quotes.

This tries to be clever about insertion. If already in a string,
it will insert \", if at the end of a string, it will glide over
the ending quote. If not in a string, it will insert \"\", and
also space out any neccessary spacing."
  (interactive)
  (shm/reparse)
  (if (shm-in-comment)
      (insert "\"")
    (let* ((current-node (shm-current-node))
           (node (if (eq 'Lit (shm-node-cons current-node))
                     (shm-actual-node)
                   current-node)))
      (cond
       ((shm-find-overlay 'shm-quarantine)
        (insert "\"\"")
        (forward-char -1))
       ;; "…|…"
       ((shm-in-string)
        (cond
         ;; "…|"
         ((= (point)
             (1- (shm-node-end node)))
          (forward-char 1))
         ;; "…|…"
         ((= (point) (shm-node-end node))
          (if (looking-back "\"")
              (shm-delimit "\"" "\"")
            (progn (insert "\""))))
         (t (let ((inhibit-read-only t))
              (shm-adjust-dependents (point) 2)
              (insert "\\\"")))))
       ;; '|'
       ((save-excursion (forward-char -1)
                        (looking-at "''"))
        (let ((inhibit-read-only t))
          (shm-adjust-dependents (point) 1)
          (insert "\"")))
       ;; anywhere
       (t
        (shm-delimit "\"" "\""))))))

(defun shm/comma (n)
  "Insert a comma. In a list it tries to help a bit by setting
the current node to the parent."
  (interactive "p")
  (if (shm-in-comment)
      (self-insert-command n)
    (let* ((current-pair (shm-current-node-pair))
           (current (cdr current-pair))
           (parent-pair (shm-node-parent current-pair))
           (parent (cdr parent-pair)))
      (cond
       ;; When inside a list, indent to the list's position with an
       ;; auto-inserted comma.
       ((eq 'List (shm-node-cons parent))
        (shm-insert-string ",")
        (shm-set-node-overlay parent-pair))
       (t
        (shm-insert-string ",")
        (shm-set-node-overlay parent-pair))))))

(defun shm/single-quote ()
  "Delimit single quotes."
  (interactive)
  (shm-delimit "'" "'"))

(defun shm/= ()
  "Insert equal."
  (interactive)
  (cond
   ((shm-literal-insertion)
    (insert "="))
   (t (unless (looking-back " ")
        (shm-insert-string " "))
      (shm-insert-string "=")
      (unless (looking-at " ")
        (shm-insert-string " ")))))

(defun shm/: ()
  "Insert colon."
  (interactive)
  (if (shm-literal-insertion)
      (call-interactively 'self-insert-command)
    (let ((current (shm-current-node)))
      (cond
       ((and current
             (or (eq (shm-node-cons current)
                     'SpliceDecl)
                 (string= (shm-node-type-name current)
                          "BangType")
                 (string= (shm-node-type-name current)
                          "FieldDecl")))
        (unless (looking-back "[ ]+")
          (insert " "))
        (unless (looking-back "::[ ]+")
          (shm-insert-string ":: a")
          (forward-word -1)
          (shm-evaporate (point) (1+ (point)))))
       (t
        (shm-insert-string ":"))))))

(defun shm/hyphen (n)
  "The - hyphen."
  (interactive "p")
  (if (and (looking-back "{")
           (looking-at "}"))
      (progn (insert "--")
             (forward-char -1))
    (self-insert-command n)))

(defun shm/hash (n)
  "The # hash."
  (interactive "p")
  (if (and (looking-back "{-")
           (looking-at "-}"))
      (progn (insert "#  #")
             (forward-char -2))
    (self-insert-command n)))

(defun shm/open-paren ()
  "Delimit parentheses."
  (interactive)
  (let ((current (shm-current-node)))
    (cond
     ((and current
           (or (string= "ExportSpec" (shm-node-type-name current))
               (string= "ImportSpec" (shm-node-type-name current))))
      (insert "()")
      (forward-char -1))
     (t
      (shm-delimit "(" ")")))))

(defun shm/open-bracket ()
  "Delimit brackets."
  (interactive)
  (shm-delimit "[" "]"))

(defun shm/open-brace ()
  "Delimit braces."
  (interactive)
  (let ((current (shm-current-node)))
    (cond
     ((and current
           (string= "Pat" (shm-node-type-name current)))
      (shm-insert-string "{}")
      (forward-char -1))
     (t
      (shm-delimit "{" "}")))))

(defun shm-delimit (open close)
  "Insert the given delimiters.

This is a special function because it will do different things
depending on the context.

If we're in a string, it just inserts OPEN. If we're in an
expression, it will insert OPEN and CLOSE and put the point
between them. It will also space out so that there is space
between previous nodes and the next. E.g.

foo|(bar)

If you hit \" at | then you will get:

foo \"\" (bar)

It saves one having to type spaces; it's obvious what to do
here."
  (cond
   ((and (shm-literal-insertion)
         (not (string= open "\"")))
    (shm-insert-string open))
   (t
    (shm/reparse)
    (let ((current (shm-actual-node)))
      (cond
       ((shm-find-overlay 'shm-quarantine)
        (if (not (or (looking-back "[ ,[({]")
                     (bolp)))
            (progn (shm-insert-string " ") 1)
          0)
        (shm-insert-string open)
        (let ((point (point)))
          (shm-insert-string close)
          (when (and (/= (point) (line-end-position))
                     (not (looking-at "[]){} ,]")))
            (shm-insert-string " "))
          (goto-char point)))
       (t
        (if (not (or (looking-back "[ ,[({]")
                     (bolp)))
            (progn (shm-insert-string " ") 1)
          0)
        (shm-insert-string open)
        (let ((point (point)))
          (shm-insert-string close)
          (when (and (/= (point) (line-end-position))
                     (not (looking-at "[]){} ,]")))
            (shm-insert-string " "))
          (goto-char point)
          (shm/init t))))))))

(defun shm-literal-insertion ()
  "Should a node have literal insertion?"
  (or (shm-in-string)
      (shm-in-comment)))

(provide 'shm-insertion)
