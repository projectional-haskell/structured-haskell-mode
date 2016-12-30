;;; shm-reformat.el ---

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

(require 'shm-ast)
(require 'hindent)

(defun shm-reformat-decl ()
  "Reformat the current declaration with `hindent/reformat-decl'
and then jump back to the right node."
  (interactive)
  (if (region-active-p)
      (call-interactively 'hindent-reformat-region)
      (let* ((current-pair (shm-current-node-pair))
         (index (car current-pair))
         (offset (- (point) (shm-node-start (cdr current-pair)))))
    (structured-haskell-mode -1)
    (hindent/reformat-decl)
    (structured-haskell-mode 1)
    (shm/reparse)
    (let ((new-current (elt (shm-decl-ast) index)))
      (goto-char (+ (shm-node-start new-current) offset))))))

(defun shm-reinsert-undefined-slots-after-fill ()
  "Replace any text matching \"\bundefined\b\" with the with the
  same text and \"evaporating slot\" slot property. Hindent seems
  to remove certain text properties."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (save-excursion
      (save-restriction
       (narrow-to-region (car bounds) (cdr bounds))
       (goto-char (point-min))
       (while (search-forward-regexp "\\bundefined\\b" nil t)
         (progn
           (delete-region (point) (save-excursion (backward-word 1) (point)))
           (shm/insert-undefined)
           (forward-word)))))))

(defadvice shm-reformat-decl (after reinsert-undefined activate)
  (shm-reinsert-undefined-slots-after-fill))

(provide 'shm-reformat)
