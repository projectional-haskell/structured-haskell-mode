;;; shm-macros.el --- Macros

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

(defmacro shm-with-fallback (fallback &rest body)
  "Perform the given action unless we're in a comment, in which
  case run the fallback function insteaad."
  `(if (shm-in-comment)
       (call-interactively ',fallback)
     (if debug-on-error
         (progn ,@body)
       (condition-case e
           (progn ,@body)
         (error
          (message "(SHM command failed, falling back to %S. Run M-: (setq debug-on-error t) to see the error.)"
                   ',fallback)
          (call-interactively ',fallback))))))

(provide 'shm-macros)
