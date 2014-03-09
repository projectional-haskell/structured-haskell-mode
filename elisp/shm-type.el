;;; shm-type.el --- Type info for nodes

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

(defun shm-present-type-info (node info)
  "Present type info to the user."
  (let ((info. (concat (shm-kill-node 'buffer-substring-no-properties node nil t)
                       " :: "
                       info)))
    (if shm-use-presentation-mode
        (if (fboundp 'haskell-present)
            (haskell-present "SHM-Node"
                             nil
                             info.)
          (message "%s" info))
      (message "%s" info))))

(defun shm-type-of-region (beg end)
  "Get a type for the region."
  (let ((types (shm-types-at-point beg)))
    (loop for type
          in types
          do (when (and (= (elt type 0) beg)
                        (= (elt type 1)
                           end))
               (return (elt type 2))))))

(defun shm-types-at-point (point)
  "Get a list of spans and types for the current point."
  (save-excursion
    (goto-char point)
    (let ((line (line-number-at-pos))
          (col (1+ (current-column)))
          (file-name (buffer-file-name)))
      (cond
       (shm-use-hdevtools
        (shm-parse-hdevtools-type-info
         (with-temp-buffer
           (call-process "hdevtools" nil t nil "type" "-g" "-fdefer-type-errors"
                         file-name
                         (number-to-string line)
                         (number-to-string col))
           (buffer-string))))))))

(defun shm-parse-hdevtools-type-info (string)
  "Parse type information from the output of hdevtools."
  (let ((lines (split-string string "\n+")))
    (loop for line
          in lines
          while (string-match "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \"\\(.+\\)\"$"
                              line)
          do (goto-char (point-min))
          collect
          (let ((start-line (string-to-number (match-string 1 line)))
                (end-line (string-to-number (match-string 3 line))))
            (vector (progn (forward-line (1- start-line))
                           (+ (line-beginning-position)
                              (1- (string-to-number (match-string 2 line)))))
                    (progn (when (/= start-line end-line)
                             (forward-line (1- (- start-line end-line))))
                           (+ (line-beginning-position)
                              (1- (string-to-number (match-string 4 line)))))
                    (match-string 5 line))))))

(defun shm-node-type-info (node)
  "Get the type of the given node."
  (shm-type-of-region (shm-node-start node)
                      (shm-node-end node)))

(provide 'shm-type)
