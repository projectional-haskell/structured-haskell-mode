;;; shm-languages.el --- Supported languages

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

(require 'shm-stack)

(defvar shm-supported-languages nil)
(make-variable-buffer-local 'shm-supported-languages)

(defun shm-supported-languages ()
  (or shm-supported-languages
      (setq shm-supported-languages
            (remove-if
             (lambda (s) (string= s ""))
             (split-string
              (if (shm-stack-project-p)
                  (shell-command-to-string "stack ghc -- --supported-languages")
                (shell-command-to-string "ghc --supported-languages"))
              "\n")))))

(provide 'shm-languages)
