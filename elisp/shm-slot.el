;;; shm-slot.el --- Slots for shm

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
(require 'shm-layout)

(require 'cl)

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

(defun shm/insert-undefined ()
  "Insert undefined."
  (interactive)
  (let ((point (point)) (bumped nil))
    (when (and (looking-back "[^[({;, ]")
               (not (bolp)))
      (shm-insert-string " ")
      (setq point (1+ point)))
    (when (and (looking-at "[^])},; ]+_*")
               (not (eolp)))
      (shm-insert-string " ")
      (forward-char -1))
    (shm-insert-string "undefined")
    (shm-evaporate point (point))
    (goto-char point)))

(defun shm/insert-underscore ()
  "Insert underscore."
  (interactive)
  (save-excursion
    (let ((point (point)))
      (when (looking-back "[a-zA-Z0-9]+_*")
        (shm-insert-string " "))
      (when (looking-at "[a-zA-Z0-9]+_*")
        (shm-insert-string " ")
        (forward-char -1))
      (shm-insert-string "_")
      (shm-evaporate point (point)))))

(defun shm-auto-insert-lambda ()
  "Insert template

\_ -> undefined
"
  (save-excursion
    (shm/insert-underscore)
    (forward-char)
    (insert " -> ")
    (shm/insert-undefined)))

(defun shm-auto-insert-do ()
  "Insert template

do {undefined}
   {undefined}
"
  (insert " ")
  (let ((point (point))
        (column (current-column)))
    (insert "undefined")
    (cond
     ((bound-and-true-p structured-haskell-repl-mode)
      (forward-word -1)
      (shm/reparse)
      (save-excursion
        (shm-evaporate (point) (+ (point) (length "undefined")))))
     (t (insert "\n")
        (indent-to column)
        (let ((next-point (point)))
          (insert "undefined")
          (goto-char point)
          (shm/reparse)
          (save-excursion
            (shm-evaporate (point) (+ (point) (length "undefined")))
            (goto-char next-point)
            (shm-evaporate (point) (+ (point) (length "undefined")))))))))

(defun shm-auto-insert-case (lambda-case)
  "Insert template

case {undefined} of
  {_} -> {undefined}

or

\\case {undefined}
  {_} -> {undefined}
"
  (let ((start (save-excursion (forward-char -1)
                               (search-backward-regexp "[^a-zA-Z0-9_]")
                               (forward-char 1)
                               (point)))
        (template (if lambda-case
                      (if (bound-and-true-p structured-haskell-repl-mode)
                          "case _ -> undefined"
                        "case\n  _ -> undefined")
                    (if (bound-and-true-p structured-haskell-repl-mode)
                        "case undefined of _ -> undefined"
                      "case undefined of\n  _ -> undefined"))))
    (shm-adjust-dependents (point) (- start (point)))
    (delete-region start (point))
    (shm-adjust-dependents (point) (length (car (last (split-string template "\n")))))
    (shm-insert-indented
     (lambda ()
       (insert template)))
    (forward-char 5)
    (shm/reparse)
    (if lambda-case
        (progn (search-forward-regexp "_" nil nil 1)
               (let ((here (1- (point))))
                 (shm-evaporate (1- (point)) (point))
                 (forward-char 4)
                 (shm-evaporate (point) (+ (point) (length "undefined")))
                 (goto-char here)))
      (save-excursion
        (shm-evaporate (point) (+ (point) (length "undefined")))
        (search-forward-regexp "_" nil nil 1)
        (shm-evaporate (1- (point)) (point))
        (forward-char 4)
        (shm-evaporate (point) (+ (point) (length "undefined")))))))

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
        (template (if (bound-and-true-p structured-haskell-repl-mode)
                      "if undefined then undefined else undefined"
                    "if undefined\n   then undefined\n   else undefined")))
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
  ;; If needs to be nested this way. Don't change it.
  (let
      ((evaporate-in (lambda ()
                       (forward-char 4)
                       (save-excursion
                         (forward-word)
                         (forward-char 1)
                         (shm-evaporate (point) (+ (point) (length "undefined")))))))
    (if (bound-and-true-p structured-haskell-repl-mode)
        (let ((points (shm-decl-points)))
          (if points
              (if (= (point) (car points))
                  (progn (shm-insert-indented
                          (lambda () (insert "let _ = undefined")))
                         (search-forward "_")
                         (shm-evaporate (1- (point)) (point))
                         (forward-word 1)
                         (forward-word -1)
                         (shm-evaporate (point) (+ (point) (length "undefined")))
                         (search-backward "_"))
                (progn (shm-insert-indented
                        (lambda () (insert "let  in undefined")))
                       (funcall evaporate-in)))
            (insert "let ")))
      (progn (shm-insert-indented
              (lambda () (insert "let \nin undefined")))
             (funcall evaporate-in))))
  (shm/reparse))

(defun shm-auto-insert-module ()
  "Insert template

module | where"
  (insert "  where")
  (backward-word 1)
  (forward-char -1))

(provide 'shm-slot)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; byte-compile-warnings: (not cl-macros)
;; End:
