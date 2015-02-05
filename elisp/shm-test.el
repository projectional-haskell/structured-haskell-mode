;;; shm-test.el --- Testing suite

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

;;; Commentary:

;; Simple test writing and running suite.

;;; Code:

(require 'shm-tests)
(require 'shm)

(defvar shm-test-eob nil)

(defun shm-test/run-all ()
  "Run all tests."
  (interactive)
  (setq  shm-colon-enabled t)
  (when (remove-if-not #'identity
                       (mapcar #'shm-test/run shm-tests))
    (message "All tests passed OK.")))

(defun shm-test/new ()
  "Make a new SHM test."
  (interactive)
  (switch-to-buffer (get-buffer-create "*shm-test*"))
  (let ((map (make-composed-keymap shm-map)))
    (define-key map (kbd "C-c C-c") 'shm-test/continue)
    (use-local-map map))
  (structured-haskell-mode t)
  (when (fboundp 'god-local-mode)
    (god-local-mode -1))
  (when (fboundp 'electric-indent-mode)
    (electric-indent-mode -1))
  (erase-buffer)
  (insert "\n")
  (setq shm-test-eob (set-marker (make-marker) (point)))
  (insert "-- Steps to create a test\n"
          "-- \n"
          "--  1. Insert the test-case setup code.\n"
          "--  2. Move the cursor to the starting point.\n"
          "--  3. Hit C-c C-c to record cursor position.\n"
          "--  4. Press F3 to begin recording the test actions.\n"
          "--  5. Do the action.\n"
          "--  6. Hit F4 to complete the action and run C-c C-c.\n"
          "--\n")
  (goto-char (point-min)))

(defun shm-test/continue ()
  "Save the cursor position in the test."
  (interactive)
  (message "last-command: %S" last-command)
  (cond
   ((eq last-command
        'kmacro-end-or-call-macro)
    (let ((point (point)))
      (save-excursion
        (goto-char (point-max))
        (insert
         (format ":finish-cursor %d\n" point)
         (format ":current-node-overlay '%S\n"
                 (let ((o shm-current-node-overlay))
                   (if o
                       (list (overlay-start o)
                             (overlay-end o))
                     nil)))
         (format ":end-buffer-content %S\n"
                 (buffer-substring-no-properties
                  (point-min) (marker-position shm-test-eob)))))
      (goto-char (point-max))
      (let ((point (point)))
        (call-interactively 'insert-kbd-macro)
        (eval (buffer-substring-no-properties point (point)))
        (delete-region point (point))
        (insert (format ":kbd %S)" last-kbd-macro))
        (shm-test/save))))
   (t
    (let ((point (point)))
      (save-excursion
        (goto-char (point-max))
        (insert
         "(list "
         (format ":name %S\n" (read-from-minibuffer "Name: "))
         (format ":start-buffer-content %S\n"
                 (buffer-substring-no-properties
                  (point-min) (marker-position shm-test-eob)))
         (format ":start-cursor %d\n" point)))))))

(defun shm-test/save ()
  "Save the test to a lisp expression."
  (emacs-lisp-mode)
  (let ((point (point)))
    (backward-sexp)
    (let ((string (buffer-substring-no-properties point (point))))
      (erase-buffer)
      (insert string)
      (indent-region (point-min)
                     (point-max))
      (backward-sexp))))

(defun shm-test/wait ()
  "Wait for a second."
  (interactive)
  (sit-for 1))

(defun shm-test/run (test)
  "Run the given test and validate it."
  (message "Testing %s..." (plist-get test :name))
  (switch-to-buffer-other-window (get-buffer-create "*shm-test*"))
  (erase-buffer)
  (kill-all-local-variables)
  (when (fboundp 'god-local-mode)
    (god-local-mode -1))
  (when (fboundp 'electric-indent-mode)
    (electric-indent-mode -1))
  (let ((customizations (plist-get test :customizations)))
    (when customizations
      (dolist (entry customizations)
        (set (make-local-variable (car entry))
             (cdr entry)))))
  (structured-haskell-mode 1)
  (insert (plist-get test :start-buffer-content))
  (goto-char (plist-get test :start-cursor))
  (shm/reparse)
  (execute-kbd-macro (plist-get test :kbd))
  (shm-test-validate test)
  (shm-mode-stop))

(defun shm-test-validate (test)
  "Validate the given test."
  (let ((name (plist-get test :name)))
    (let ((actual (buffer-substring-no-properties (point-min) (point-max)))
          (expected (plist-get test :end-buffer-content)))
      (unless (string= actual expected)
        (error "\nTest failed, differing buffer contents.

Original:

%s

Expected (quoted):

%s

Actual (quoted):

%s\n"
               (plist-get test :start-buffer-content)
               (shm-test-exact-quote expected)
               (shm-test-exact-quote actual))))
    (let ((actual (point))
          (expected (plist-get test :finish-cursor)))
      (unless (= actual expected)
        (error "\nTest failed, differing cursor positions.

Expected:

%d

Actual:

%d\n"
               expected actual))))
  (kill-buffer)
  t)

(defun shm-test-exact-quote (s)
  "Quote a string exactly, so you can see any details or differences in whitespace."
  (mapconcat 'identity
             (mapcar (lambda (l)
                       (concat "\"" l "\""))
                     (split-string s "\n"))
             "\n"))

(provide 'shm-test)

;;; shm-test.el ends here
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
