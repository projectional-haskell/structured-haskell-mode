;;; shm-debug.el --- Debugging utilities

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

(require 'shm-layout)

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

(defun shm/describe-node (&optional node)
  "Present a description of the current node in the minibuffer.

Very useful for debugging and also a bit useful for newbies."
  (interactive)
  (let ((node (or node (shm-current-node))))
    (if node
        (message "%s" (shm-node-description node))
      (error "No current node."))))

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
                (shm-node-string node))
      (format "Node type: “%s” (no more info)"
              (shm-node-type-name node)))))

(defun shm-node-string (node)
  "Get the string of the NODE."
  (save-excursion
    (shm-kill-node 'buffer-substring-no-properties
                   node
                   nil
                   t)))

(provide 'shm-debug)
