;;; shm-ast.el --- AST generation functions

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

(require 'shm-globals)
(require 'shm-nodes)
(require 'shm-ast-documentation)
(require 'shm-customizations)
(require 'shm-yank-kill)

(require 'cl)

(defvar shm-decl-asts nil
  "This is partly an optimization and partly for more
functionality. We could parse the whole module, but that would be
wasteful and expensive to lookup nodes every time we want a
node. So it's cheaper to have the granularity of lookup start at
the declaration's point and the node's span.

Second it's better because a module may have unparseable content
in it, but that doesn't mean we don't want structured editing to
stop working on declarations that are fine. I've found in my use
of SHM that this is a common use-case worth taking into account.")

(defun shm-node-description (node)
  "Generate a description of the given node suitable to be put in
  the minibuffer. If no documentation can be found, it generates
  a reasonable string instead."
  (let* ((type-doc (assoc (shm-node-type-name node)
                          shm-ast-documentation))
         (con-doc (assoc (symbol-name (shm-node-cons node))
                         (cddr type-doc))))
    (if type-doc
        (format "Node type: “%s”: %s, case: %s\n%s"
                (nth 0 type-doc)
                (nth 1 type-doc)
                (if con-doc
                    (format "“%s”: %s"
                            (nth 0 con-doc)
                            (nth 1 con-doc))
                  (format "“%s” (no more info)"
                          (shm-node-cons node)))
                (save-excursion
                  (shm-kill-node 'buffer-substring-no-properties
                                 node
                                 nil
                                 t)))
      (format "Node type: “%s” (no more info)"
              (shm-node-type-name node)))))

(defun shm-set-decl-ast (point ast)
  "Store the given decl AST at the given POINT. If there is
already an AST for a decl at the given point then remove that one
and instate this one."
  (setq shm-decl-asts
        (cons
         (cons (set-marker (make-marker) point) ast)
         (remove-if (lambda (pair)
                      (when (= (marker-position (car pair))
                               point)
                        (set-marker (car pair) nil)
                        t))
                    shm-decl-asts)))
  ast)

(defun shm-get-decl-ast (start end &optional reparse)
  "Get the AST of the declaration starting at POINT."
  (let ((pair (car (remove-if-not (lambda (pair)
                                    (= (marker-position (car pair))
                                       start))
                                  shm-decl-asts))))
    (if (and (not reparse)
             pair)
        (cdr pair)
      (progn
        (when (or (/= start shm-last-parse-start)
                  (/= end shm-last-parse-end))
          (setq shm-last-parse-start start)
          (setq shm-last-parse-end end)
          (let ((ast (shm-get-nodes (shm-get-ast "decl"
                                                 start
                                                 end)
                                    start
                                    end)))
            (if ast
                (progn (setq shm-lighter " SHM")
                       (when pair
                         (shm-delete-markers pair))
                       (shm-set-decl-ast start ast)
                       ;; Delete only quarantine overlays.
                       (shm-delete-overlays (point-min) (point-max) 'shm-quarantine)
                       (shm/init)
                       ast)
              (progn
                (when shm-display-quarantine
                  (shm-quarantine-overlay start end))
                (setq shm-lighter " SHM!")
                nil))))))))

(defun shm-delete-markers (decl)
  "Delete the markers in DECL."
  (mapc #'shm-node-delete-markers
        (cdr decl)))

(defun shm-get-ast (type start end)
  "Get the AST for the given region at START and END. Parses with TYPE.

This currently launches a fresh process and uses this buffer
nonsense, for any parse, which sucks, but is fast enough _right
now_. Later on a possibility to make this much faster is to have
a persistent running parser server and than just send requests to
it, that should bring down the roundtrip time significantly, I'd
imagine."
  (let ((message-log-max nil)
        (buffer (current-buffer)))
    (when (> end (1+ start))
      (with-temp-buffer
        (let ((temp-buffer (current-buffer)))
          (with-current-buffer buffer
            (condition-case e
                (call-process-region start
                                     end
                                     shm-program-name
                                     nil
                                     temp-buffer
                                     nil
                                     "parse"
                                     type)
              ((file-error)
               (error "Unable to find structured-haskell-mode executable! See README for help.")))))
        (read (buffer-string))))))

(defun shm-check-ast (type start end)
  "Check whether the region of TYPE from START to END parses.

This doesn't generate or return an AST, it just checks whether it
parses."
  (let ((message-log-max nil)
        (buffer (current-buffer)))
    (with-temp-buffer
      (let ((temp-buffer (current-buffer)))
        (with-current-buffer buffer
          (call-process-region start
                               end
                               shm-program-name
                               nil
                               temp-buffer
                               nil
                               "check"
                               ;; In other words, always parse with
                               ;; the more generic “decl” when
                               ;; something starts at column 0,
                               ;; because HSE distinguishes between a
                               ;; “declaration” and an import, a
                               ;; module declaration and a language
                               ;; pragma.
                               (if (save-excursion (goto-char start)
                                                   (= (point) (line-beginning-position)))
                                   "decl"
                                 type))))
      (string= "" (buffer-string)))))

(defun shm-get-nodes (ast start end)
  "Get the nodes of the given AST.

We convert all the line-col numbers to Emacs points and then
create markers out of them. We also store the type of the node,
e.g. Exp, and the case of the node, e.g. Lit or Case or Let,
which is helpful for doing node-specific operations like
indentation.

Any optimizations welcome."
  (let* ((start-end (cons start end))
         (start-column (save-excursion (goto-char start)
                                       (current-column))))
    (cond ((vectorp ast)
           (save-excursion
             (map 'vector
                  (lambda (node)
                    (vector
                     (elt node 0)
                     (elt node 1)
                     (progn (goto-char (car start-end))
                            (forward-line (1- (elt node 2)))
                            ;; This trick is to ensure that the first
                            ;; line's columns are offsetted for
                            ;; regions that don't start at column
                            ;; zero.
                            (goto-char (+ (if (= (elt node 2) 1)
                                              start-column
                                            0)
                                          (1- (+ (point) (elt node 3)))))
                            (let ((marker (set-marker (make-marker) (point))))
                              marker))
                     (progn (goto-char (car start-end))
                            (forward-line (1- (elt node 4)))
                            ;; Same logic as commented above.
                            (goto-char (+ (if (= (elt node 4) 1)
                                              start-column
                                            0)
                                          (1- (+ (point) (elt node 5)))))
                            ;; This avoids the case of:
                            (while (save-excursion (goto-char (line-beginning-position))
                                                   (or (looking-at "[ ]+-- ")
                                                       (looking-at "[ ]+$")))
                              (forward-line -1)
                              (goto-char (line-end-position)))
                            (let ((marker (set-marker (make-marker) (point))))
                              (set-marker-insertion-type marker t)
                              marker))))
                  ast)))
          (t nil))))

(provide 'shm-ast)
