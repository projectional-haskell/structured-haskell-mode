;;; shm-edit-string.el --- Editing strings

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

(require 'shm-insert-del)
(require 'shm-layout)

(defvar shm-string-buffer nil
  "The buffer of the string node that's currently being edited.")

(defvar shm-string-node nil
  "The string node that's currently being edited.")

(defun shm/edit-string ()
  "Edit the string at point."
  (interactive)
  (let ((current (shm-current-node))
        (buffer (current-buffer))
        (string (shm-kill-node 'buffer-substring-no-properties nil nil t)))
    (goto-char (shm-node-start current))
    (switch-to-buffer (get-buffer-create "*shm-string*"))
    (erase-buffer)
    (insert
     (replace-regexp-in-string
      "\\\\\"" "\""
      (replace-regexp-in-string
       "\\\\n" "\n"
       (replace-regexp-in-string
        "^\"\\(.*\\)\"$" "\\1"
        (replace-regexp-in-string
         "\\\\\n\\\\" ""
         string)))))
    (shm-edit-string-mode)
    (set (make-local-variable 'shm-string-node)
         current)
    (set (make-local-variable 'shm-string-buffer)
         buffer)
    (goto-char (point-min))))

(define-derived-mode shm-edit-string-mode
  text-mode "String"
  "Major mode for editing string content from a Haskell string.")

(define-key shm-edit-string-mode-map (kbd "C-c C-c") 'shm-finish-editing-string)

(defun shm-finish-editing-string ()
  "Take the contents of the buffer and insert it back into the
original node in the Haskell buffer, replacing the old one."
  (interactive)
  (let ((finish-string (buffer-string))
        (buffer shm-string-buffer))
    (quit-window)
    (switch-to-buffer buffer)
    (shm/delete)
    (insert "\"\"")
    (forward-char -1)
    (save-excursion
      (font-lock-fontify-region (line-beginning-position)
                                (line-end-position)))
    (shm-insert-indented (lambda () (insert finish-string)))
    (forward-char -1)))

(provide 'shm-edit-string)
