;;; shm-generic.el --- Generic mode functions

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

(require 'shm-types)
(require 'shm-ast)

(defun shm/reparse ()
  "Re-parse the current node.

This is used on the reparsing timer, but also on commands that
really need accurate AST information *right now*, so this will
force a reparse immediately (if necessary)."
  (interactive)
  (shm-decl-ast t)
  (when (/= shm-last-point (point))
    (shm-set-node-overlay)))

(defun shm/mark-node ()
  "Set the active mark to the current node."
  (interactive)
  (let ((current (shm-current-node)))
    (goto-char (shm-node-start current))
    (set-mark (shm-node-end current))))

(defun shm/test-exe ()
  "Test that the executable is working properly."
  (interactive)
  (let ((region (shm-decl-points)))
    (when (get-buffer "*shm-scratch-test*")
      (with-current-buffer
          (switch-to-buffer "*shm-scratch-test*")
        (erase-buffer)))
    (call-process-region (car region)
                         (cdr region)
                         shm-program-name
                         nil
                         "*shm-scratch-test*"
                         nil
                         "parse"
                         "decl")
    (switch-to-buffer "*shm-scratch-test*")
    (when (save-excursion (goto-char (point-min))
                          (looking-at "structured-haskell-mode:"))
      (insert "\nNote: If you got a parse error for valid code
that is using fairly new (read: couple years) a GHC extension,
you are probably hitting the fact that haskell-src-exts doesn't
parse a bunch of newer GHC extensions. SHM does not do any
parsing itself, it uses HSE. There are some patches in the HSE
repo, provided as pull requests, which you can try applying to a
local copy of HSE and then recompile SHM with the new version.

See also: https://github.com/haskell-suite/haskell-src-exts/issues/19

And: https://github.com/chrisdone/structured-haskell-mode/blob/master/src/Main.hs"))))

(defun shm/type-of-node ()
  (interactive)
  (let ((current (shm-current-node)))
    (cond
     ((or (string= (shm-node-type-name current) "Exp")
          (string= (shm-node-type-name current) "Decl")
          (string= (shm-node-type-name current) "Pat")
          (string= (shm-node-type-name current) "QOp"))
      (let ((type-info (shm-node-type-info current)))
        (if type-info
            (shm-present-type-info current type-info)
          (if (and shm-type-info-fallback-to-ghci
                   (fboundp 'haskell-process-do-type))
              (haskell-process-do-type)
            (error "Unable to get type information for that node.")))))
     ((and (string= (shm-node-type-name current) "Name")
           (let ((parent-name (shm-node-type-name (cdr (shm-node-parent (shm-current-node-pair))))))
             (or (string= parent-name "Match")
                 (string= parent-name "Decl"))))
      (let* ((node (cdr (shm-node-parent (shm-current-node-pair))))
             (type-info (shm-node-type-info node)))
        (if type-info
            (shm-present-type-info node type-info)
          (if (and shm-type-info-fallback-to-ghci
                   (fboundp 'haskell-process-do-type))
              (haskell-process-do-type)
            (error "Unable to get type information for that node (tried the whole decl, too).")))))
     (t (error "Not an expression, operator, pattern binding or declaration.")))))

(defun shm/describe-node (&optional node)
  "Present a description of the current node in the minibuffer.

Very useful for debugging and also a bit useful for newbies."
  (interactive)
  (let ((node (or node (shm-current-node))))
    (if node
        (message "%s" (shm-node-description node))
      (error "No current node."))))

(provide 'shm-generic)
