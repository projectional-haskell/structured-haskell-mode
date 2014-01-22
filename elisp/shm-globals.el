;;; shm-globals.el --- Global variables shared among modules

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

(defvar shm-lighter " SHM?"
  "The lighter for structured Haskell mode.")

(defvar shm-last-point 0
  "When moving around, the current node overlay will update
  according to where you are. But often you can shrink/expand the
  scope of the current node. This variable lets us avoid the node
  being reset by realising we haven't actually moved the point.")

(defvar shm-parsing-timer nil
  "The timer used to re-parse every so often. The idle time can
  be configured with `shm-idle-timeout'.")

(defvar shm-last-parse-start 0
  "This is used to avoid unnecessary work, if the start of the
  declaration hasn't changed, and the end (see
  `shm-last-parse-end') since we last parsed, don't bother
  re-parsing.")

(defvar shm-last-parse-end 0
  "See `shm-last-parse-start' for explanation.")

(defvar shm-last-yanked (list 0 0)
  "When yanking, some text will be inserted, when popping a
  yank (i.e. with M-y), you need to be able to erase the previous
  yank. This is simply a region.")

(provide 'shm-globals)
