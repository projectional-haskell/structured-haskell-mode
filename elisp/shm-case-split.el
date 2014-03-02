;;; shm-case-split.el --- Case splitting functionality

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

;;; Commentary:

;; Produces a list of case alternatives from a sum type data
;; declaration.

;;; Code:

(require 'shm)
(require 'haskell-process)

(defun shm-case-split-insert-pattern (alts)
  "Takes the first alt in ALTS and inserts a pattern match for
  it."
  (when (car alts)
    (let ((alt (car alts)))
      (when (> (cdr alt) 0)
        (insert "("))
      (insert (car alt))
      (loop for i from 1 to (cdr alt)
            do (progn (insert " _")
                      (shm-evaporate (1- (point)) (point))))
      (when (> (cdr alt) 0)
        (insert ")")))))

(defun shm-case-split-insert-alts (alts)
  "Inserts case alts for the given ALTS. It will create
evaporating slots for each part. E.g.

case x of
  |

for data Maybe a = Just a | Nothing will insert

case x of
  Just _ -> undefined
  Nothing -> undefined

Where the _ and undefineds are evaporating slots."
  (let ((column (current-column)))
    (loop for alt in alts
          do (progn (when (/= column (current-column))
                      (newline)
                      (indent-to column))
                    (insert (car alt))
                    (loop for i from 1 to (cdr alt)
                          do (progn (insert " _")
                                    (shm-evaporate (1- (point)) (point))))
                    (insert " -> undefined")
                    (shm-evaporate (- (point) (length "undefined"))
                                   (point))))))

(defun shm-case-split-alts-from-data-decl (string)
  "Given a data declaration STRING, generate a list of alternatives."
  (with-temp-buffer
    (insert (replace-regexp-in-string
             "[a-zA-Z0-9]+-[0-9.]+:"
             ""
             string))
    (text-mode)
    (structured-haskell-mode)
    (setq shm-last-parse-start (point-max))
    (setq shm-last-parse-end (point-min))
    (shm/reparse)
    (mapcar #'shm-case-split-name-and-arity
            (shm-case-split-get-constructors))))

(defun shm-case-split-generate-alt (cons)
  "Generate an alt from the given NODE-PAIR."
  (let ((name (car cons))
        (arity (cdr cons)))
    (format "%s%s"
            name
            (apply 'concat
                   (loop for i from 1 to arity
                         collect " _")))))

(defun shm-case-split-name-and-arity (node-pair)
  "Get the constructor name and arity of the given constructor NODE-PAIR."
  (let* ((parent (shm-node-child-pair node-pair))
         (name-node (shm-node-child parent)))
    (goto-char (shm-node-end name-node))
    (cons (shm-node-string name-node)
          (or (when (/= (shm-node-end name-node)
                        (shm-node-end (cdr parent)))
                (shm/forward-node)
                (shm/reparse)
                (let ((n 0)
                      (last-node 0)
                      (current-pair (shm-current-node-pair)))
                  (while (and (/= (point) (point-max))
                              current-pair
                              (= (car parent)
                                 (car (shm-node-parent current-pair))))
                    (when (/= (car current-pair)
                              last-node)
                      (setq n (1+ n))
                      (setq last-node (car current-pair)))
                    (unless (= (point)
                               (point-max))
                      (shm/forward-node)
                      (shm/reparse)
                      (setq current-pair (shm-current-node-pair))))
                  n))
              0))))

(defun shm-case-split-get-constructors ()
  "Get a list of constructors."
  (goto-char (point-min))
  (or (search-forward "= " nil t 1)
      (error "Couldn't find any constructors (searched for '=')."))
  (let ((conses (list)))
    (while (/= (point) (point-max))
      (let ((cons (shm-case-split-get-constructor)))
        (when cons
          (setq conses (cons cons conses)))))
    (reverse conses)))

(defun shm-case-split-get-constructor ()
  "Get the constructor at point."
  (shm/reparse)
  (let ((cons-pair (shm-node-ancestor-at-point (shm-current-node-pair)
                                               (point))))
    (goto-char (shm-node-end (cdr cons-pair)))
    (or (search-forward "| " nil t 1)
        (goto-char (point-max)))
    cons-pair))

;; Backend based on haskell-process.el

(defun haskell-process-get-data-type (name)
  "Get the data type definition of the given name."
  (let ((reply
         (haskell-process-queue-sync-request (haskell-process)
                                             (format ":i %s\n" name))))
    (car (split-string reply "[\n\t ]+-- Defined "))))

(defun shm/case-split (name)
  "Do a case split on NAME at point."
  (interactive (list (read-from-minibuffer "Type: ")))
  (save-excursion
    (let ((column (current-column)))
      (insert "case undefined ")
      (shm-evaporate (- (point) (+ 1 (length "undefined")))
                     (- (point) 1))
      (insert "of\n")
      (indent-to (+ column 2))
      (shm-case-split-insert-alts
       (shm-case-split-alts-from-data-decl
        (haskell-process-get-data-type name))))))

(defun shm/expand-pattern (name)
  "Expand a pattern match on a data type."
  (interactive (list (read-from-minibuffer "Type: ")))
  (save-excursion
    (shm-case-split-insert-pattern
     (shm-case-split-alts-from-data-decl
      (haskell-process-get-data-type name)))))

(provide 'shm-case-split)
