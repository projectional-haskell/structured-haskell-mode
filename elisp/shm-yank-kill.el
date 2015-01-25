;;; shm-yank-kill.el --- Yanking/killing operations

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

(require 'shm-macros)
(require 'shm-layout)

(defun shm/mark-node ()
  "Set the active mark to the current node."
  (interactive)
  (let ((current (shm-current-node)))
    (goto-char (shm-node-start current))
    (set-mark (shm-node-end current))))

(defun shm/kill-region (beg end)
  "Kill the region, and save it in the clipboard."
  (interactive "r")
  (shm-kill-region nil beg end nil))

(defun shm/copy-region (beg end)
  "Copy the region, and save it in the clipboard."
  (interactive "r")
  (save-excursion
    (shm-kill-region 'clipboard-kill-ring-save beg end t)))

(defun shm/kill-line ()
  "Kill everything possible to kill after point before the end of
the line.

Successive kills will also work, for example:

do |foo
   bar
   mu

Hitting C-k C-k C-k here will killall three lines, and then C-y
will insert them back verbatim."
  (interactive)
  (shm-with-fallback
   kill-line
   (shm/reparse)
   (cond
    ((looking-at "^[ ]+$")
     (delete-region (point) (line-end-position)))
    ((= (line-end-position) (line-beginning-position))
     (delete-char -1)
     (forward-char 1))
    ((and (shm-in-string)
          (not (= (point)
                  (shm-node-start (shm-current-node)))))
     (let ((current (shm-current-node)))
       (if (and (> (shm-node-end current)
                   (line-end-position))
                (save-excursion (goto-char (line-end-position))
                                (looking-back "\\\\")))
           (kill-region (point) (1- (line-end-position)))
         (kill-region (point)
                      (1- (shm-node-end current))))))
    ((and (= (point) (line-end-position))
          (not (looking-at "\n[^ ]")))
     (let ((column (current-column)))
       (delete-region (point)
                      (save-excursion (forward-line 1)
                                      (goto-char (+ (line-beginning-position)
                                                    column))))
       (shm-kill-to-end-of-line t)))
    ((shm-current-node)
     (shm-kill-to-end-of-line))
    (t (kill-line)))))

(defun shm/kill-node ()
  "Kill the current node."
  (interactive)
  (shm-kill-node))

(defun shm/yank ()
  "Yank from the kill ring and insert indented with `shm-insert-indented'."
  (interactive)
  (shm-with-fallback
   yank
   ;; This avoids merging two identifiers together accidentally.
   (unless (or (shm-in-comment)
               (shm-in-string))
     (when (looking-back "[a-zA-Z0-9]+_*")
       (shm-insert-string " "))
     (when (and (looking-at "[a-zA-Z0-9]+_*")
                (not (shm-evaporate-before-p)))
       (shm-insert-string " ")
       (forward-char -1)))
   (shm-insert-indented #'clipboard-yank)))

(defun shm/yank-pop ()
  "Yank from the kill ring and insert indented with `shm-insert-indented'."
  (interactive)
  (shm-with-fallback
   yank-pop
   (if (not (eq last-command 'yank))
       (error "Previous command was not a yank (error from shm/yank-pop)"))
   (shm-insert-indented #'yank-pop)))

(defun shm/backward-kill-word ()
  "Kill the word backwards."
  (interactive)
  (let ((to-be-deleted (save-excursion (backward-word)
                                       (point))))
    (save-excursion
      (shm-adjust-dependents (point) (* -1 (- (point) to-be-deleted))))
    (backward-kill-word 1)))

(provide 'shm-yank-kill)
