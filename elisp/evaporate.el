(defface evaporate-face
  '((((class color)) :foreground "#666666"))
  "Face for text that will evaporate when modified/overwritten."
  :group 'evaporate)

(defun evaporate (beg end)
  "Make the region evaporate when typed over."
  (interactive "r")
  (let ((o (make-overlay beg end nil nil nil)))
    (overlay-put o 'evaporate-overlay t)
    (overlay-put o 'face 'evaporate-face)
    (overlay-put o 'evaporate t)
    (overlay-put o 'priority 2)
    (overlay-put o 'modification-hooks '(evaporate-modification-hook))
    (overlay-put o 'insert-in-front-hooks '(evaporate-insert-before-hook))))

(defun evaporate-modification-hook (o changed beg end &optional len)
  "Remove the overlay after a modification occurs."
  (let ((inhibit-modification-hooks t))
    (when (and changed
               (overlay-start o))
      (delete-region (overlay-start o)
                     (overlay-end o))
      (delete-overlay o))))

(defun evaporate-insert-before-hook (o changed beg end &optional len)
  "Remove the overlay before inserting something at the start."
  (let ((inhibit-modification-hooks t))
    (when (and (not changed)
               (overlay-start o))
      (delete-region (overlay-start o)
                     (overlay-end o))
      (delete-overlay o))))

(provide 'evaporate)
