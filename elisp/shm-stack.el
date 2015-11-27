;;; shm-stack.el --- Stack integration

;; Copyright (c) 2015 Chris Done. All rights reserved.

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

(defvar shm-stack-project-p nil)
(make-variable-buffer-local 'shm-stack-project-p)

(defun shm-stack-project-p ()
  "Is this buffer in a stack project?"
  (or (bound-and-true-p shm-stack-project-p)
      (setq shm-stack-project-p
            (not (not (shm-stack-find-file))))))

(defun shm-stack-find-file (&optional dir)
  "Copied from `haskell-yaml-find-file'."
  (let ((use-dir (or dir default-directory)))
    (while (and use-dir (not (file-directory-p use-dir)))
      (setq use-dir (file-name-directory (directory-file-name use-dir))))
    (when use-dir
      (catch 'found
        (let ((user (nth 2 (file-attributes use-dir)))
              ;; Abbreviate, so as to stop when we cross ~/.
              (root (abbreviate-file-name use-dir)))
          ;; traverse current dir up to root as long as file owner doesn't change
          (while (and root (equal user (nth 2 (file-attributes root))))
            (let ((yaml-file (shm-stack-find-yaml root)))
              (when yaml-file
                (throw 'found yaml-file)))

            (let ((proot (file-name-directory (directory-file-name root))))
              (if (equal proot root) ;; fix-point reached?
                  (throw 'found nil)
                (setq root proot))))
          nil)))))

(defun shm-stack-find-yaml (dir &optional allow-multiple)
  "Copied from `haskell-yaml-find-pkg-desc'."
  (let* ((yaml-files
          (cl-remove-if 'file-directory-p
                        (cl-remove-if-not 'file-exists-p
                                          (directory-files dir t ".\\.yaml\\'")))))
    (cond
     ((= (length yaml-files) 1) (car yaml-files)) ;; exactly one candidate found
     (allow-multiple yaml-files) ;; pass-thru multiple candidates
     (t nil))))

(provide 'shm-stack)
