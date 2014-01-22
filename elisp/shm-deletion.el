;;; shm-deletion.el --- Deletion operations

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

(require 'shm-indentation)
(require 'shm-nodes)
(require 'shm-macros)
(require 'shm-generic)
(require 'shm-globals)
(require 'shm-insertion)

(defun shm/backward-kill-word ()
  "Kill the word backwards."
  (interactive)
  (let ((to-be-deleted (save-excursion (backward-word)
                                       (point))))
    (save-excursion
      (shm-appropriate-adjustment-point)
      (shm-adjust-dependents (point) (* -1 (- (point) to-be-deleted))))
    (backward-kill-word 1)))

(defun shm/delete ()
  "Delete the current node."
  (interactive)
  (let ((current (shm-current-node))
        (inhibit-read-only t))
    (delete-region (shm-node-start current)
                   (shm-node-end current))))

(defun shm/del ()
  "Character deletion handler.

Generally, we delete things in the current node. BUT, there are
some things that we shouldn't delete, because they would cause
parse errors that are rarely useful. For example:

    (|case x of _ -> _) -- where | indicates cursor.

"
  (interactive)
  (shm-with-fallback
   delete-backward-char
   (cond
    ;; These cases are “gliders”. They simply move over the character
    ;; backwards. These could be handled all as one regular
    ;; expression, but in the interest of clarity—for now—they are left
    ;; as separate cases.
    ((and (shm-in-string)
          (looking-back "^[ ]*\\\\"))
     (let ((here (point)))
       (delete-region (search-backward-regexp "\\\\$")
                      here)))
    ((and (looking-back "{-[ ]*")
          (looking-at "[ ]*-}"))
     (delete-region (search-backward-regexp "-")
                    (progn (forward-char 1)
                           (search-forward-regexp "-"))))
    ((and (looking-back "^{-#[ ]*")
          (looking-at "[ ]*#-}$"))
     (delete-region (search-backward-regexp "#")
                    (progn (forward-char 1)
                           (search-forward-regexp "#"))))
    ((looking-back "[()]") (shm-delete-or-glide "(" ")"))
    ((looking-back "[[]") (shm-delete-or-glide "\\[" "\\]"))
    ((looking-back "[]]") (shm-delete-or-glide "\\[" "\\]"))
    ((looking-back "[{}]") (shm-delete-or-glide "{" "}"))
    ((looking-back "[\"]") (shm-delete-or-glide "\"" "\""))
    ;; These kind of patterns block the parens of syntaxes that would
    ;; otherwise break everything, so, "if", "of", "case", "do",
    ;; etc. if deleted.
    ((and (looking-back "[^A-Zaz0-9_]do ?")
          (not (or (eolp)
                   (looking-at "[])}]"))))
     nil) ; do nothing
    ((and (looking-back " <-")
          (not (or (eolp)
                   (looking-at "[])}]"))))
     (forward-char -3))
    ((and (looking-back " <- ")
          (not (or (eolp)
                   (looking-at "[])}]"))))
     (forward-char -4))
    ((looking-back "[^A-Zaz0-9_]of ?")
     (search-backward-regexp "[ ]*of"))
    ((or (looking-at "of$")
         (looking-at "of "))
     (forward-char -1))
    ((looking-back "[_ ]-> ?") (forward-char -3))
    ((looking-at "-> ?")
     (forward-char -1))
    ((looking-back "[^A-Zaz0-9_]then ?")
     (search-backward-regexp "[^ ][ ]*then")
     (unless (or (looking-at "$") (looking-at " "))
       (forward-char 1)))
    ((looking-back "[^A-Zaz0-9_]else ?")
     (search-backward-regexp "[^ ][ ]*else")
     (unless (or (looking-at "$") (looking-at " "))
       (forward-char 1)))
    ((looking-back "^module ?")
     (when (looking-at "[ ]*where$")
       (delete-region (line-beginning-position) (line-end-position))))
    ((looking-back "[^A-Zaz0-9_]if ?")
     nil) ; do nothing
    ((looking-back "[^A-Zaz0-9_]case ?")
     nil) ; do nothing
    ((and (looking-at "= ")
          (looking-back " "))
     (forward-char -1))
    ((or (and (looking-back " = ")
              (not (looking-at "$"))
              (not (looking-at " ")))
         (and (looking-back "=")
              (looking-at " ")))
     (search-backward-regexp "[ ]+=[ ]*"
                             (line-beginning-position)
                             t
                             1)
     (when (looking-back " ")
       (when (search-backward-regexp "[^ ]" (line-beginning-position)
                                     t 1)
         (forward-char 1))))
    ;; This is the base case, we assume that we can freely delete
    ;; whatever we're looking back at, and that the node will be able
    ;; to re-parse it.
    (t (save-excursion
         (shm-appropriate-adjustment-point)
         (shm-adjust-dependents (point) -1))
       (shm-delete-char))))
  (shm/init t))

(defun shm-delete-or-glide (open close)
  "Delete the given OPEN/CLOSE delimiter, or simply glide over it
  if it isn't empty."
  (cond
   ;; If the delimiters are empty, we can delete the whole thing.
   ((shm-delimiter-empty open close)
    (let ((inhibit-read-only t))
      (shm-adjust-dependents (point) -2)
      (delete-region (1- (point))
                     (1+ (point)))))
   ;; If the delimiters aren't empty and we're in a literal, then go
   ;; ahead and elete the character.
   ((and (shm-literal-insertion)
         (not (= (point) (1+ (shm-node-start (shm-current-node))))))
    (shm-delete-char))
   ;; Otherwise just glide over the character.
   (t
    (when (looking-back close)
      (forward-char -1)))))

(defun shm-delete-char ()
  "Delete a character backwards or delete the region, if there is
one active."
  (if (region-active-p)
      (delete-region (region-beginning)
                     (region-end))
    (delete-region (1- (point))
                   (point))))

(defun shm-delimiter-empty (open close)
  "Is the current expression delimited by OPEN and CLOSE empty?"
  (and (looking-back open)
       (not (save-excursion (forward-char (* -1 (length open)))
                            (looking-back "\\\\")))
       (looking-at close)))

(provide 'shm-deletion)
