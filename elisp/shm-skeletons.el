;;; shm-skeletons.el --- Skeleton syntax templates

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

(defun shm-auto-insert-stmt (type)
  "Insert template

do x <- |
   {undefined}
"
  (let* ((current (shm-current-node))
         (column (save-excursion
                   (case type
                     ('let (backward-word 1)
                       (current-column))
                     ('qualifier
                      (cond
                       ((eq 'Do (shm-node-cons current))
                        (goto-char (shm-node-start current))
                        (forward-char 2)
                        (search-forward-regexp "[^ \n]")
                        (1- (current-column)))
                       (t (goto-char (shm-node-start current))
                          (current-column))))))))
    (unless (save-excursion
              (let ((current-line (line-number-at-pos)))
                (forward-line 1)
                (goto-char (+ (line-beginning-position)
                              column))
                (and (not (bolp))
                     (/= current-line (line-number-at-pos))
                     (= (point)
                        (save-excursion (back-to-indentation)
                                        (point))))))
      (save-excursion
        (newline)
        (indent-to column)
        (insert "undefined")
        (forward-word -1)
        (shm/reparse)
        (shm-evaporate (point)
                   (progn (forward-word 1)
                          (point)))))
    (insert " ")))

(defun shm-auto-insert-do ()
  "Insert template

do {undefined}
   {undefined}
"
  (insert " ")
  (let ((point (point))
        (column (current-column)))
    (insert "undefined")
    (newline)
    (indent-to column)
    (let ((next-point (point)))
      (insert "undefined")
      (goto-char point)
      (shm/reparse)
      (save-excursion
        (shm-evaporate (point) (+ (point) (length "undefined")))
        (goto-char next-point)
        (shm-evaporate (point) (+ (point) (length "undefined")))))))

(defun shm-auto-insert-case ()
  "Insert template

case {undefined} of
  {_} -> {undefined}
"
  (let ((start (save-excursion (forward-char -1)
                               (search-backward-regexp "[^a-zA-Z0-9_]")
                               (forward-char 1)
                               (point)))
        (template "case undefined of\n  _ -> undefined"))
    (shm-adjust-dependents (point) (- start (point)))
    (delete-region start (point))
    (shm-adjust-dependents (point) (length (car (last (split-string template "\n")))))
    (shm-insert-indented
     (lambda ()
       (insert template)))
    (forward-char 5)
    (shm/reparse)
    (save-excursion
      (shm-evaporate (point) (+ (point) (length "undefined")))
      (search-forward-regexp "_" nil nil 1)
      (shm-evaporate (1- (point)) (point))
      (forward-char 4)
      (shm-evaporate (point) (+ (point) (length "undefined"))))))

(defun shm-auto-insert-if ()
  "Insert template

if {undefined}
   then {undefined}
   else {undefined}

or

if {undefined} then {undefined} else {undefined}

if inside parentheses."
  (let ((start (save-excursion (forward-char -1)
                               (search-backward-regexp "[^a-zA-Z0-9_]")
                               (forward-char 1)
                               (point)))
        (template (if (looking-at "$")
                      "if undefined\n   then undefined\n   else undefined"
                    "if undefined then undefined else undefined")))
    (shm-adjust-dependents (point) (- start (point)))
    (delete-region start (point))
    (shm-adjust-dependents (point) (length (car (last (split-string template "\n")))))
    (shm-insert-indented
     (lambda ()
       (insert template)))
    (forward-char 3)
    (save-excursion
      (shm-evaporate (point) (+ (point) (length "undefined")))
      (search-forward-regexp "then ")
      (shm-evaporate (point) (+ (point) (length "undefined")))
      (search-forward-regexp "else ")
      (shm-evaporate (point) (+ (point) (length "undefined"))))))

(defun shm-auto-insert-let ()
  "Insert template

let | in {undefined}"
  (delete-region (- (point) 3) (point))
  (shm-insert-indented (lambda () (insert "let \nin undefined")))
  (forward-char 4)
  (save-excursion
    (forward-word)
    (forward-char 1)
    (shm-evaporate (point) (+ (point) (length "undefined")))))

(defun shm-auto-insert-module ()
  "Insert template

module | where"
  (insert "  where")
  (backward-word 1)
  (forward-char -1))

(provide 'shm-skeletons)
