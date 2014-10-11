;;; shm-fold.el --- Code folding.

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

(defun shm-fold ()
  "Fold either the region or the node at point."
  (interactive)
  (if (region-active-p)
      (shm-fold-region (region-beginning)
                       (region-end))
    (let ((current (shm-current-node)))
      (shm-fold-region (shm-node-start current)
                       (shm-node-end current)))))

(defun shm-fold-toggle-decl ()
  "Toggle the folding or unfolding of the declaration."
  (interactive)
  (let* ((points (shm-decl-points))
         (o (car (remove-if-not (lambda (o)
                                  (overlay-get o 'folded-decl))
                                (overlays-in (car points)
                                             (cdr points))))))
    (if o
        (delete-overlay o)
      (shm-fold-decl))))

(defun shm-fold-decl ()
  "Fold the current declaration."
  (interactive)
  (let* ((points (shm-decl-points))
         (beg (save-excursion (goto-char (car points))
                              (line-end-position)))
         (end (cdr points)))
    (when (> end beg)
      (shm-fold-region beg end 'folded-decl))))

(defun shm-fold-region (beg end &optional prop)
  "Hide region."
  (let ((o (make-overlay beg end)))
    (overlay-put o 'invisible t)
    (overlay-put o 'intangible t)
    (overlay-put o 'after-string "...")
    (overlay-put o 'hide-region t)
    (overlay-put o prop t)))

(defun shm-fold-region-undo ()
  "Undo the hidden region at point."
  (interactive)
  (mapcar (lambda (o)
            (when (overlay-get o 'hide-region)
              (delete-overlay o)))
          (overlays-in (- (point) 1)
                       (+ (point) 1))))

(provide 'shm-fold)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
