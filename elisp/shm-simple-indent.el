;;; shm-simple-indent.el --- Simple indentation

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

(defun shm/simple-indent ()
  "Space out to under next visible indent point.
Indent points are positions of non-whitespace following whitespace in
lines preceeding point.  A position is visible if it is to the left of
the first non-whitespace of every nonblank line between the position and
the current line.  If there is no visible indent point beyond the current
column, `tab-to-tab-stop' is done instead."
  (interactive)
  (let* ((start-column (current-column))
         (invisible-from nil)           ; `nil' means infinity here
         (indent
          (catch 'shm-simple-indent-break
            (save-excursion
              (while (progn (beginning-of-line)
                            (not (bobp)))
                (forward-line -1)
                (if (not (looking-at "[ \t]*\n"))
                    (let ((this-indentation (current-indentation)))
                      (if (or (not invisible-from)
                              (< this-indentation invisible-from))
                          (if (> this-indentation start-column)
                              (setq invisible-from this-indentation)
                            (let ((end (line-beginning-position 2)))
                              (move-to-column start-column)
                              ;; Is start-column inside a tab on this line?
                              (if (> (current-column) start-column)
                                  (backward-char 1))
                              (or (looking-at "[ \t]")
                                  (skip-chars-forward "^ \t" end))
                              (skip-chars-forward " \t" end)
                              (let ((col (current-column)))
                                (throw 'shm-simple-indent-break
                                       (if (or (= (point) end)
                                               (and invisible-from
                                                    (> col invisible-from)))
                                           invisible-from
                                         col)))))))))))))
    (if indent
        (let ((opoint (point-marker)))
          (indent-line-to indent)
          (if (> opoint (point))
              (goto-char opoint))
          (set-marker opoint nil))
      (tab-to-tab-stop))))

(defun shm/simple-indent-backtab ()
  "Indent backwards. Dual to `shm-simple-indent'."
  (interactive)
  (let ((current-point (point))
        (i 0)
        (x 0))
    (goto-char (line-beginning-position))
    (save-excursion
      (while (< (point) current-point)
        (shm/simple-indent)
        (setq i (+ i 1))))
    (while (< x (- i 1))
      (shm/simple-indent)
      (setq x (+ x 1)))))

(defun shm/simple-indent-newline-same-col ()
  "Make a newline and go to the same column as the current line."
  (interactive)
  (let ((point (point)))
    (let ((start-end
           (save-excursion
             (let* ((start (line-beginning-position))
                    (end (progn (goto-char start)
                                (search-forward-regexp
                                 "[^ ]" (line-end-position) t 1))))
               (when end (cons start (1- end)))))))
      (if start-end
          (progn (insert "\n")
                 (insert (buffer-substring-no-properties
                          (car start-end) (cdr start-end))))
        (insert "\n")))))

(defun shm/simple-indent-newline-indent ()
  "Make a newline on the current column and indent on step."
  (interactive)
  (shm/simple-indent-newline-same-col)
  (insert (make-string (shm-indent-spaces) ? )))

(provide 'shm-simple-indent)
