;;; shm-evaporate.el --- Evaporating overlays

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

;; Support for evaporating pieces of code.

;;; Code:

(defface shm-evaporate-face
  '((((class color)) :foreground "#666666"))
  "Face for text that will evaporate when modified/overwritten."
  :group 'shm-evaporate)

(defun shm-evaporate (beg end)
  "Make the region evaporate when typed over."
  (interactive "r")
  (let ((o (make-overlay beg end nil nil nil)))
    (overlay-put o 'shm-evaporate-overlay t)
    (overlay-put o 'face 'shm-evaporate-face)
    (overlay-put o 'shm-evaporate t)
    (overlay-put o 'priority 2)
    (overlay-put o 'modification-hooks '(shm-evaporate-modification-hook))
    (overlay-put o 'insert-in-front-hooks '(shm-evaporate-insert-before-hook))))

(defun shm-evaporate-modification-hook (o changed beg end &optional len)
  "Remove the overlay after a modification occurs."
  (let ((inhibit-modification-hooks t))
    (when (and changed
               (overlay-start o))
      (shm-evaporate-delete-text o beg end)
      (delete-overlay o))))

(defun shm-evaporate-insert-before-hook (o changed beg end &optional len)
  "Remove the overlay before inserting something at the start."
  (let ((inhibit-modification-hooks t))
    (when (and (not changed)
               (overlay-start o))
      (shm-evaporate-delete-text o beg end)
      (delete-overlay o))))

(defun shm-evaporate-delete-text (o beg end)
  "Delete the text associated with the evaporating slot."
  (unless (eq this-command 'undo)
    (delete-region (overlay-start o)
                   (overlay-end o))))

(provide 'shm-evaporate)

;;; shm-evaporate.el ends here
