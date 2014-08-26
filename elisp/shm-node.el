;;; shm-node.el --- Node functions

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

(require 'shm-customizations)
(require 'shm-ast-documentation)

(defun shm-node-type (n)
  "Get the AST type of N."
  (elt n 0))

(defun shm-node-type-name (n)
  "Get just the constructor name part of N.

This doesn't always return the correct thing, e.g. [Foo Bar] will
return [Foo. It's just a convenience function to get things like
Case or whatnot"
  (nth 0 (split-string (elt n 0) " ")))

(defun shm-node-cons (n)
  "Get the constructor name of N."
  (elt n 1))

(defun shm-node-start (n)
  "Get the start position of N in its buffer."
  (marker-position (elt n 2)))

(defun shm-node-end (n)
  "Get the end position of N in its buffer."
  (marker-position (elt n 3)))

(defun shm-node-set-start (n x)
  "Set the start position of N."
  (set-marker (elt n 2) x))

(defun shm-node-set-end (n x)
  "Set the end position of N."
  (set-marker (elt n 3) x))

(defun shm-node-delete-markers (n)
  "Set the markers to NIL, which is about the best we can do for
deletion. The markers will be garbage collected eventually."
  (set-marker (elt n 2) nil)
  (set-marker (elt n 3) nil))

(defun shm-node-start-column (n)
  "Get the starting column of N."
  (save-excursion (goto-char (shm-node-start n))
                  (current-column)))

(defun shm-node-start-line (n)
  "Get the starting line of N."
  (save-excursion (goto-char (shm-node-start n))
                  (line-number-at-pos)))

(defun shm-node-indent-column (n)
  "Get the starting column of N."
  (+ (shm-node-start-column n)
     (if (or (string= "Tuple" (shm-node-cons n))
             (string= "Paren" (shm-node-cons n))
             (string= "List" (shm-node-cons n)))
         1
       0)))

(defun shm-node-end-column (n)
  "Get the end column of N."
  (save-excursion (goto-char (shm-node-end n))
                  (current-column)))

(defun shm-node-empty (n)
  "Is the node empty of any text?"
  (= (shm-node-start n)
     (shm-node-end n)))

(defun shm-node-pp (n)
  "Pretty print the node."
  (format "%s: %S: %d—%d"
          (shm-node-type-name n)
          (shm-node-cons n)
          (shm-node-start n)
          (shm-node-end n)))

(defun shm-node-string (n)
  "Get the string of the region spanned by the node."
  (buffer-substring-no-properties (shm-node-start n)
                                  (shm-node-end n)))

(defun shm-node-app-p (node)
  "Is the given node an application of some kind?"
  (or (eq (shm-node-cons node) 'App)
      (eq (shm-node-cons node) 'InfixApp)
      (eq (shm-node-cons node) 'TyApp)))

(defun shm-node-paren-p (node)
  "Is the given node a paren of some kind?"
  (or (eq (shm-node-cons node) 'Paren)
      (eq (shm-node-cons node) 'PParen)
      (eq (shm-node-cons node) 'TyParen)))

(provide 'shm-node)
