;;; shm-in.el --- Are we in some thing.

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

(defun shm-in-comment ()
  "Are we currently in a comment?"
  (or (and (eq 'font-lock-comment-delimiter-face
               (get-text-property (point) 'face))
           ;; This is taking liberties, but I'm not too sad about it.
           (not (save-excursion (goto-char (line-beginning-position))
                                (looking-at "{-"))))
      (eq 'font-lock-doc-face
          (get-text-property (point) 'face))
      (and (eq 'font-lock-comment-face
               (get-text-property (point) 'face))
           (not (save-excursion (goto-char (line-beginning-position))
                                (looking-at "{-"))))
      (save-excursion (goto-char (line-beginning-position))
                      (looking-at "^\-\- "))))

(defun shm-in-string ()
  "Are we in a string?"
  (save-excursion
    (when (looking-at "\"")
      (forward-char -1))
    (eq 'font-lock-string-face
        (get-text-property (point) 'face))))

(defun shm-in-char ()
  "Are we in a char literal?"
  (save-excursion
    (and (looking-at "'")
         (looking-back "'"))))

(defun shm-literal-insertion ()
  "Should a node have literal insertion?"
  (or (shm-in-string)
      (shm-in-char)
      (shm-in-comment)))

(provide 'shm-in)
