;;; shm-overlays.el --- Error and current node overlays

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


;; Requirements

(require 'shm-globals)


;; Buffer locals

(defvar shm-current-node-overlay nil
  "Overlay to highlight the current node.")


;; Functions

(defun shm/init (&optional force-renew)
  "Initialize the current node overlay at point.

FORCE-RENEW would be used when the buffer has changed and
therefore the current overlay should be re-initialized."
  (interactive)
  (when force-renew
    (setq shm-current-node-overlay nil))
  (shm-set-node-overlay))

(defun shm-delete-overlays (start end type)
  "Delete overlays of the given type. This is used for both
current overlay and quarantines."
  (mapc (lambda (o)
          (when (overlay-get o type)
            (delete-overlay o)))
        (overlays-in start end)))

(defun shm-find-overlay (type)
  "Find overlays at point."
  (remove-if-not (lambda (o) (overlay-get o type))
                 (overlays-in (point-min) (point-max))))

(defun shm-current-overlay (start end node-pair)
  "Make the overlay for current node at START to END, setting the
NODE-PAIR in the overlay."
  (let ((o (make-overlay start end nil nil t)))
    (overlay-put o 'shm-current-overlay t)
    (overlay-put o 'face 'shm-current-face)
    (overlay-put o 'node-pair node-pair)
    (overlay-put o 'priority 1)
    o))

(defun shm-quarantine-overlay (start end)
  "Make a quarantine from START to END."
  (let ((o (make-overlay start end nil nil t)))
    (overlay-put o 'shm-quarantine t)
    (overlay-put o 'face 'shm-quarantine-face)
    (overlay-put o 'priority 0)
    o))

(defun shm-set-node-overlay (&optional node-pair jump-direction)
  "Set the current overlay for the current node. Optionally pass
NODE-PAIR to use the specific node-pair (index + node)."
  (setq shm-current-node-overlay nil)
  (shm-delete-overlays (point-min)
                       (point-max)
                       'shm-current-overlay)
  (let* ((node-pair (or node-pair
                        (shm-current-node-pair)))
         (node (cdr node-pair)))
    (when jump-direction
      (if (eq jump-direction 'end)
          (goto-char (shm-node-end node))
        (goto-char (shm-node-start node))))
    (setq shm-last-point (point))
    (setq shm-current-node-overlay
          (when node
            (shm-current-overlay (shm-node-start node)
                                 (shm-node-end node)
                                 node-pair)))))

(provide 'shm-overlays)
