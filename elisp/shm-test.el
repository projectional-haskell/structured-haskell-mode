(require 'shm)
(require 'shm-tests)

(defvar shm-test-eob nil)

(defun shm-test/run-all ()
  "Run all tests."
  (interactive)
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
  (message "Running %s ..." (plist-get test :name))
  (switch-to-buffer-other-window (get-buffer-create "*shm-test*"))
  (erase-buffer)
  (when (fboundp 'god-local-mode)
    (god-local-mode -1))
  (structured-haskell-mode 1)
  (insert (plist-get test :start-buffer-content))
  (goto-char (plist-get test :start-cursor))
  (shm/reparse)
  (execute-kbd-macro (plist-get test :kbd))
  (shm-test-validate test))

(defun shm-test-validate (test)
  "Validate the given test."
  (assert (string= (buffer-substring-no-properties (point-min) (point-max))
                   (plist-get test :end-buffer-content)))
  (assert (= (plist-get test :finish-cursor)
             (point)))
  (message "%s: OK" (plist-get test :name))
  (kill-buffer)
  t)

(provide 'shm-test)


;;; shm-test.el ends here
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
