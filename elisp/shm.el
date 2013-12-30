;;; structured-haskell-mode --- Structured editing for Haskell

;; Copyright (c) 2013, Chris Done. All rights reserved.

;; Author:    Chris Done <chrisdone@gmail.com>
;; Created:   19-Oct-2013
;; Version:   1.0.0
;; Keywords:  development, haskell, structured
;; Stability: unstable

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;;     * Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.

;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.

;;     * Neither the name of Chris Done nor the names of other
;;       contributors may be used to endorse or promote products
;;       derived from this software without specific prior written
;;       permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; A minor mode for adding structured editing to Haskell.

;;; Code:


;; Requirements

(require 'shm-ast-documentation)
(require 'cl)


;; Groups

(defgroup shm nil
  "Structured editing mode for Haskell"
  :group 'haskell)


;; Mode

(defvar shm-map
  (let ((map (make-sparse-keymap)))
    ;; Insertion
    (define-key map (kbd "\"") 'shm/double-quote)
    (define-key map (kbd "(") 'shm/open-paren)
    (define-key map (kbd "M-(") 'shm/wrap-parens)
    (define-key map (kbd "[") 'shm/open-bracket)
    (define-key map (kbd "{") 'shm/open-brace)
    (define-key map (kbd ",") 'shm/comma)
    (define-key map (kbd ":") 'shm/:)
    (define-key map (kbd "SPC") 'shm/space)
    ;; Indentation
    (define-key map (kbd "C-j") 'shm/newline-indent)
    (define-key map (kbd "M-)") 'paredit-close-round-and-newline)
    (define-key map (kbd "C-c C-^") 'shm/swing-up)
    (define-key map (kbd "C-c C-j") 'shm/swing-down)
    ;; Deletion
    (define-key map (kbd "DEL") 'shm/del)
    (define-key map (kbd "<deletechar>") 'shm/delete)
    (define-key map (kbd "M-^") 'shm/delete-indentation)
    (define-key map (kbd "M-DEL") 'shm/backward-kill-word)
    (define-key map (kbd "C-<backspace>") 'shm/backward-kill-word)
    ;; Killing & yanking
    (define-key map (kbd "C-k") 'shm/kill-line)
    (define-key map (kbd "M-k") 'shm/kill-node)
    (define-key map (kbd "C-w") 'shm/kill-region)
    (define-key map (kbd "C-M-k") 'shm/kill-node)
    (define-key map (kbd "C-y") 'shm/yank)
    (define-key map (kbd "M-y") 'shm/yank-pop)
    ;; Navigation
    (define-key map (kbd "C-M-f") 'shm/forward-node)
    (define-key map (kbd "C-M-b") 'shm/backward-node)
    (define-key map (kbd "M-a") 'shm/goto-parent)
    (define-key map (kbd ")") 'shm/close-paren)
    (define-key map (kbd "]") 'shm/close-paren)
    (define-key map (kbd "}") 'shm/close-paren)
    (define-key map (kbd "M-}") 'shm/forward-paragraph)
    (define-key map (kbd "M-{") 'shm/backward-paragraph)
    (define-key map (kbd "C-M-SPC") 'shm/mark-node)
    (define-key map (kbd "C-c C-w") 'shm/goto-where)
    ;; Splitting, slurping, barfing, etc.
    (define-key map (kbd "C-+") 'shm/add-operand)
    (define-key map (kbd "M-r") 'shm/raise)
    map)
  "Structural editing operations keymap. Any key bindings in this
  map are intended to be only structural operations which operate
  with the tree in mind.")

(defvar shm-lighter " SHM?"
  "The lighter for structured Haskell mode.")

(define-minor-mode structured-haskell-mode
  "Structured editing for Haskell."
  :lighter shm-lighter
  :keymap shm-map
  (if structured-haskell-mode
      (shm-mode-start)
    (shm-mode-stop)))


;; Macros

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
          (message "Falling back to %S" ',fallback)
          (call-interactively ',fallback))))))


;; Internal mode functions

(defun shm-mode-start ()
  "Start the minor mode."
  (set (make-local-variable 'shm-decl-asts)
       nil)
  (set (make-local-variable 'shm-current-node-overlay)
       nil)
  (add-hook 'post-self-insert-hook 'shm-post-self-insert nil t)
  (unless shm-parsing-timer
    (setq shm-parsing-timer
          (run-with-idle-timer shm-idle-timeout t 'shm-reparsing-timer))))

(defun shm-post-self-insert ()
  "Self-insertion handler."
  (save-excursion
    (shm-appropriate-adjustment-point)
    (forward-char -1)
    (shm-adjust-dependents (point) 1)))

(defun shm-mode-stop ()
  "Stop the minor mode. Restore various settings and clean up any
state that will hopefully be garbage collected."
  ;; Kill the timer.
  (cancel-timer shm-parsing-timer)
  (setq shm-parsing-timer nil)
  ;; Delete all markers.
  (mapc (lambda (pair)
          (mapc #'shm-node-delete-markers
                (cdr pair))
          (set-marker (car pair) nil))
        shm-decl-asts)
  ;; Delete all overlays.
  (shm-delete-overlays (point-min) (point-max) 'shm-current-overlay)
  (shm-delete-overlays (point-min) (point-max) 'shm-quarantine)
  ;; Reset variables.
  (setq shm-decl-asts nil)
  (setq shm-current-node-overlay nil)
  (setq shm-last-parse-start 0)
  (setq shm-last-parse-end 0)
  (setq shm-last-point 0))

(defun shm-reparsing-timer ()
  "Re-parse the tree on the idle timer."
  (when structured-haskell-mode
    (shm/reparse)))

(defun shm-indent-spaces ()
  "Get the number of spaces to indent."
  (if (boundp 'haskell-indent-spaces)
      haskell-indent-spaces
    shm-indent-spaces))


;; Faces

(defface shm-quarantine-face
  '((((class color)) :background "#443333"))
  "Face for quarantines."
  :group 'shm)

(defface shm-current-face
  '((((class color)) :background "#373737"))
  "Face for the current node."
  :group 'shm)


;; Customization

(defcustom shm-auto-insert-skeletons
  t
  "Auto-insert skeletons for case, if, etc."
  :group 'shm
  :type 'bool)

(defcustom shm-program-name
  "structured-haskell-mode"
  "The path to call for parsing Haskell syntax."
  :group 'shm
  :type 'string)

(defcustom shm-kill-zone-name
  "*shm-kill-zone*"
  "The name of the buffer to use for the kill zone."
  :group 'shm
  :type 'string)

(defcustom shm-indent-spaces
  (if (boundp 'haskell-indent-spaces)
      haskell-indent-spaces
    2)
  "The number of spaces to indent by default."
  :group 'shm
  :type 'string)

(defcustom shm-lambda-indent-style
  nil
  "Specify a particular style for indenting lambdas?"
  :group 'shm
  :type '(choice (const leftmost-parent) (const nil)))

(defcustom shm-use-presentation-mode
  nil
  "Use haskell-presentation-mode?"
  :group 'shm
  :type 'bool)

(defcustom shm-display-quarantine
  t
  "Display quarantine?"
  :group 'shm
  :type 'bool)

(defcustom shm-use-hdevtools
  nil
  "Use hdevtools for type information?"
  :group 'shm
  :type 'bool)

(defcustom shm-type-info-fallback-to-ghci
  t
  "Fallback to GHCi when the type-info backend returns nothing?"
  :group 'shm
  :type 'bool)

(defcustom shm-idle-timeout
  0.2
  "Number of seconds before re-parsing."
  :group 'shm
  :type 'string)




;; Globals

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

(defvar shm-last-point 0
  "When moving around, the current node overlay will update
  according to where you are. But often you can shrink/expand the
  scope of the current node. This variable lets us avoid the node
  being reset by realising we haven't actually moved the point.")

(defvar shm-last-yanked (list 0 0)
  "When yanking, some text will be inserted, when popping a
  yank (i.e. with M-y), you need to be able to erase the previous
  yank. This is simply a region.")


;; Buffer-local variables

(defvar shm-decl-asts nil
  "This is partly an optimization and partly for more
functionality. We could parse the whole module, but that would be
wasteful and expensive to lookup nodes every time we want a
node. So it's cheaper to have the granularity of lookup start at
the declaration's point and the node's span.

Second it's better because a module may have unparseable content
in it, but that doesn't mean we don't want structured editing to
stop working on declarations that are fine. I've found in my use
of SHM that this is a common use-case worth taking into account.")

(defvar shm-current-node-overlay nil
  "Overlay to highlight the current node.")


;; Public commands

;;; Generic node operations

(defun shm/init (&optional force-renew)
  "Initialize the current node overlay at point.

FORCE-RENEW would be used when the buffer has changed and
therefore the current overlay should be re-initialized."
  (interactive)
  (when force-renew
    (setq shm-current-node-overlay nil))
  (shm-set-node-overlay))

(defun shm/reparse ()
  "Re-parse the current node.

This is used on the reparsing timer, but also on commands that
really need accurate AST information *right now*, so this will
force a reparse immediately (if necessary)."
  (interactive)
  (shm-decl-ast t)
  (when (/= shm-last-point (point))
    (shm-set-node-overlay)))

(defun shm/mark-node ()
  "Set the active mark to the current node."
  (interactive)
  (let ((current (shm-current-node)))
    (goto-char (shm-node-start current))
    (set-mark (shm-node-end current))))

(defun shm/test-exe ()
  "Test that the executable is working properly."
  (interactive)
  (let ((region (shm-decl-points)))
    (call-process-region (car region)
                         (cdr region)
                         shm-program-name
                         nil
                         "*shm-scratch-test*"
                         nil
                         "parse"
                         "decl")
    (switch-to-buffer "*shm-scratch-test*")))

(defun shm/type-of-node ()
  (interactive)
  (let ((current (shm-current-node)))
    (cond
     ((or (string= (shm-node-type-name current) "Exp")
          (string= (shm-node-type-name current) "Decl")
          (string= (shm-node-type-name current) "Pat")
          (string= (shm-node-type-name current) "QOp"))
      (let ((type-info (shm-node-type-info current)))
        (if type-info
            (shm-present-type-info current type-info)
          (if shm-type-info-fallback-to-ghci
              (haskell-process-do-type)
            (error "Unable to get type information for that node.")))))
     ((and (string= (shm-node-type-name current) "Name")
           (let ((parent-name (shm-node-type-name (cdr (shm-node-parent (shm-current-node-pair))))))
             (or (string= parent-name "Match")
                 (string= parent-name "Decl"))))
      (let* ((node (cdr (shm-node-parent (shm-current-node-pair))))
             (type-info (shm-node-type-info node)))
        (if type-info
            (shm-present-type-info node type-info)
          (if shm-type-info-fallback-to-ghci
              (haskell-process-do-type)
            (error "Unable to get type information for that node (tried the whole decl, too).")))))
     (t (error "Not an expression, operator, pattern binding or declaration.")))))

(defun shm/describe-node (&optional node)
  "Present a description of the current node in the minibuffer.

Very useful for debugging and also a bit useful for newbies."
  (interactive)
  (message "%s" (shm-node-description (or node (shm-current-node)))))

;;; Insertion

(defun shm/wrap-parens ()
  "Wrap the node in parentheses."
  (interactive)
  (let ((line (line-number-at-pos))
        (node (shm-current-node)))
    (save-excursion
      (goto-char (shm-node-start node))
      (insert "(")
      (goto-char (shm-node-end node))
      (when (/= line (line-number-at-pos))
        (indent-rigidly (shm-node-start node)
                        (shm-node-end node)
                        1))
      (insert ")"))
    (forward-char 1)))

(defun shm/space ()
  "Insert a space but sometimes do something more clever, like
  inserting skeletons."
  (interactive)
  (if (bound-and-true-p god-local-mode)
      (god-mode-self-insert)
    (cond
     ((shm-in-comment)
      (insert " "))
     ((and shm-auto-insert-skeletons
           (looking-back "[^a-zA-Z0-9_]case"))
      (let ((start (save-excursion (forward-char -1)
                                   (search-backward-regexp "[^a-zA-Z0-9_]")
                                   (forward-char 1)
                                   (point)))
            (template "case  of
  _ -> undefined"))
        (shm-adjust-dependents (point) (- start (point)))
        (delete-region start (point))
        (shm-adjust-dependents (point) (length (car (last (split-string template "\n")))))
        (shm-insert-indented
         (lambda ()
           (insert template)))
        (forward-char 5)))
     ((and shm-auto-insert-skeletons
           (looking-back "[^a-zA-Z0-9_]if"))
      (let ((start (save-excursion (forward-char -1)
                                   (search-backward-regexp "[^a-zA-Z0-9_]")
                                   (forward-char 1)
                                   (point)))
            (template (if (looking-at "$")
                          "if \n   then undefined\n   else undefined"
                        "if  then undefined else undefined")))
        (shm-adjust-dependents (point) (- start (point)))
        (delete-region start (point))
        (shm-adjust-dependents (point) (length (car (last (split-string template "\n")))))
        (shm-insert-indented
         (lambda ()
           (insert template)))
        (forward-char 3)))
     ((and shm-auto-insert-skeletons
           (looking-back "[^a-zA-Z0-9_]let")
           (let ((current (shm-current-node)))
             (not (or (eq 'Do (shm-node-cons current))
                      (string= "Stmt" (shm-node-type-name current))))))
      (delete-region (- (point) 3) (point))
      (shm-insert-indented (lambda () (insert "let \nin undefined")))
      (forward-char 4))
     (t (shm-insert-string " ")))))

(defun shm/double-quote ()
  "Insert double quotes.

This tries to be clever about insertion. If already in a string,
it will insert \", if at the end of a string, it will glide over
the ending quote. If not in a string, it will insert \"\", and
also space out any neccessary spacing."
  (interactive)
  (shm/reparse)
  (if (shm-in-comment)
      (insert "\"")
    (let* ((current-node (shm-current-node))
           (node (if (eq 'Lit (shm-node-cons current-node))
                     (shm-actual-node)
                   current-node)))
      (cond
       ((shm-find-overlay 'shm-quarantine)
        (insert "\"\"")
        (forward-char -1))
       ;; "…|…"
       ((shm-in-string)
        (cond
         ;; "…|"
         ((= (point)
             (1- (shm-node-end node)))
          (forward-char 1))
         ;; "…|…"
         ((= (point) (shm-node-end node))
          (if (looking-back "\"")
              (shm-delimit "\"" "\"")
            (progn (insert "\""))))
         (t (let ((inhibit-read-only t))
              (shm-adjust-dependents (point) 2)
              (insert "\\\"")))))
       ;; '|'
       ((save-excursion (forward-char -1)
                        (looking-at "''"))
        (let ((inhibit-read-only t))
          (shm-adjust-dependents (point) 1)
          (insert "\"")))
       ;; anywhere
       (t
        (shm-delimit "\"" "\""))))))

(defun shm/comma ()
  "Insert a comma. In a list it tries to help a bit by setting
the current node to the parent."
  (interactive)
  (let* ((current-pair (shm-current-node-pair))
         (current (cdr current-pair))
         (parent-pair (shm-node-parent current-pair))
         (parent (cdr parent-pair))
         (inhibit-read-only t))
    (cond
     ;; When inside a list, indent to the list's position with an
     ;; auto-inserted comma.
     ((eq 'List (shm-node-cons parent))
      (insert ",")
      (shm-set-node-overlay parent-pair))
     (t
      (insert ",")
      (shm-set-node-overlay parent-pair)))))

(defun shm/single-quote ()
  "Delimit single quotes."
  (interactive)
  (shm-delimit "'" "'"))

(defun shm/= ()
  "Insert equal."
  (interactive)
  (cond
   ((shm-literal-insertion)
    (insert "="))
   (t (unless (looking-back " ")
        (shm-insert-string " "))
      (shm-insert-string "=")
      (unless (looking-at " ")
        (shm-insert-string " ")))))

(defun shm/: ()
  "Insert colon."
  (interactive)
  (let ((current (shm-current-node)))
    (cond
     ((eq (shm-node-cons current)
          'SpliceDecl)
      (unless (looking-back "[ ]+")
        (insert " "))
      (unless (looking-back "::[ ]+")
        (shm-insert-string ":: ")))
     (t
      (shm-insert-string ":")))))

(defun shm/open-paren ()
  "Delimit parentheses."
  (interactive)
  (shm-delimit "(" ")"))

(defun shm/open-bracket ()
  "Delimit brackets."
  (interactive)
  (shm-delimit "[" "]"))

(defun shm/open-brace ()
  "Delimit braces."
  (interactive)
  (let ((current (shm-current-node)))
    (cond
     ((and current
           (string= "Pat" (shm-node-type-name current)))
      (insert "{}")
      (forward-char -1))
     (t
      (shm-delimit "{" "}")))))

;;; Indentation

(defun shm/delete-indentation ()
  "Send the node up one line."
  (interactive)
  (if (looking-back "^[ ]+")
      (cond
       ((or (looking-at "then[] [{}\"'()]")
            (looking-at "else[] [{}\"'()]"))
        (delete-indentation))
       ((looking-at "[ ]*$")
        (delete-indentation))
       (t (let ((string (shm-kill-node 'buffer-substring-no-properties)))
            (delete-indentation)
            (insert " ")
            (shm-insert-indented
             (lambda ()
               (insert string))))))
    (delete-indentation)))

(defun shm/swing-down ()
  "Swing the children of the current node downwards.

hai = do foo bar
         mu zot

With the cursor on `do', this will produce:

hai = do
  foo bar
  mu zot
"
  (interactive)
  (let* ((current-pair (shm-current-node-pair))
         (current (cdr current-pair)))
    (cond
     ((eq (shm-node-cons current)
          'Do)
      (let ((swing-string
             (shm-kill-node 'buffer-substring-no-properties
                            current
                            (shm-node-start (shm-node-child current-pair)))))
        (shm/newline-indent)
        (shm-insert-indented (lambda () (insert swing-string)))))
     ((eq (shm-node-cons current)
          'Var)
      (let* ((next-pair (shm-node-next current-pair))
             (parent-pair (shm-node-parent current-pair))
             (start (shm-node-start-column (cdr parent-pair))))
        (let ((swing-string
               (shm-kill-region 'buffer-substring-no-properties
                                (shm-node-start (cdr next-pair))
                                (shm-node-end (cdr parent-pair))
                                nil)))
          (shm/reparse)
          (forward-char -1)
          (newline)
          (indent-to (+ (shm-indent-spaces)
                        start))
          (shm-insert-indented (lambda () (insert swing-string))))))
     (t
      (error "Don't know how to swing that kind of expression.")))))

(defun shm/swing-up ()
  "Swing the children of the current node upwards.

hai = do
  foo bar
  mu zot

With the cursor on `do', this will produce:

hai = do foo bar
         mu zot
"
  (interactive)
  (let* ((current-pair (shm-current-node-pair))
         (current (cdr current-pair)))
    (cond
     ((eq (shm-node-cons current)
          'Do)
      (let ((swing-string
             (shm-kill-node 'buffer-substring-no-properties
                            current
                            (shm-node-start (shm-node-child current-pair)))))
        (delete-indentation)
        (if (looking-at " ")
            (forward-char 1)
          (insert " "))
        (shm-insert-indented (lambda () (insert swing-string)))))
     (t
      (error "Don't know how to swing that kind of expression.")))))

(defun shm/newline-indent ()
  "Make a newline and indent, making sure to drag anything down, re-indented
  with it."
  (interactive)
  (if (and (looking-at "[^])}\"]") ;; This is a cheap solution. It
           ;; could use node boundaries
           ;; instead.
           (not (looking-at "$"))
           (looking-back " "))
      ;; If there's some stuff trailing us, then drag that with us.
      (let ((newline-string (shm-kill-node 'buffer-substring-no-properties))
            (point (point)))
        (shm-newline-indent)
        (shm-insert-indented
         (lambda ()
           (insert newline-string))))
    ;; Otherwise just do the indent.
    (shm-newline-indent)))

(defun shm/goto-where ()
  "Either make or go to a where clause of the current right-hand-side."
  (interactive)
  (let ((node-pair (shm-current-node-pair))
        (vector (shm-decl-ast)))
    (loop for i
          downfrom (car node-pair)
          to -1
          until (or (= i -1)
                    (let ((node (elt vector i)))
                      (and (string= "Rhs"
                                    (shm-node-type-name node))
                           (<= (shm-node-start node)
                               (shm-node-start (cdr node-pair)))
                           (>= (shm-node-end node)
                               (shm-node-end (cdr node-pair))))))
          finally (return
                   (when (>= i 0)
                     (let ((rhs (elt vector i)))
                       (goto-char (shm-node-end rhs))
                       (cond
                        ((looking-at "[\n ]*where")
                         (search-forward-regexp "where[ \n]*"))
                        (t
                         (unless (= (line-beginning-position) (point))
                           (newline))
                         (indent-to
                          (+ 2
                             (shm-node-start-column
                              (cdr (shm-node-parent (cons i rhs))))))
                         (insert "where ")
                         (let ((point (point)))
                           (unless (looking-at "\n\n")
                             (insert "\n"))
                           (goto-char point))))))))))

;;; Navigation

(defun shm/forward-paragraph ()
  "Go forward one declaration."
  (interactive)
  (unless (/= (point)
              (goto-char (cdr (shm-decl-points))))
    (search-forward-regexp "[^\n ]" nil t 1)
    (backward-char)))

(defun shm/backward-paragraph ()
  "Go backward one declaration."
  (interactive)
  (unless (/= (point)
              (goto-char (car (shm-decl-points))))
    (search-backward-regexp "[^\n ]" nil t 1)
    (forward-char)))

(defun shm/close-paren ()
  "Either insert a close paren or go to the end of the node."
  (interactive)
  (shm-with-fallback
   self-insert-command
   (if (shm-literal-insertion)
       (shm-insert-string ")")
     (shm/goto-parent-end))))

(defun shm/goto-parent-end ()
  "Set the current node overlay to the parent node, but go to the
  end rather than the start."
  (interactive)
  (shm/goto-parent nil 'end))

(defun shm/forward-node ()
  "Go forward by node, i.e. go to the next of the current node. If
we're already at the end of the current node, jump to the next
node."
  (interactive)
  (let* ((current-pair (shm-current-node-pair))
         (current (cdr current-pair)))
    (if (= (point) (shm-node-end current))
        (let ((next-pair (shm-node-next current-pair)))
          (goto-char (shm-node-start (cdr next-pair))))
      (goto-char (shm-node-end current)))))

(defun shm/backward-node ()
  "Go backward by node, i.e. go to the previous of the current node. If
we're already at the start of the current node, jump to the previous
node."
  (interactive)
  (let* ((current-pair (shm-current-node-pair))
         (current (cdr current-pair)))
    (if (= (point) (shm-node-start current))
        (let ((prev-pair (shm-node-previous current-pair)))
          (goto-char (shm-node-start (cdr prev-pair))))
      (goto-char (shm-node-start current)))))

(defun shm/goto-parent (&optional node-pair direction)
  "Set the current node overlay to the parent node-pair"
  (interactive)
  (let ((direction (or direction 'start)))
    (if shm-current-node-overlay
        (let* ((o shm-current-node-overlay)
               (parent-pair (shm-node-parent (or node-pair
                                                 (shm-current-workable-node)))))
          (when parent-pair
            (let ((parent (cdr parent-pair)))
              (if (and o
                       (overlay-buffer o)
                       (>= (shm-node-start parent)
                           (overlay-start o))
                       (<= (shm-node-end parent)
                           (overlay-end o)))
                  (shm/goto-parent parent-pair direction)
                (shm-set-node-overlay parent-pair direction)))))
      (when node-pair
        (shm-set-node-overlay node-pair direction)))))

;;; Splitting, slurping, barfing

(defun shm/add-operand ()
  "When in an infix application, figure out the operator and add
a new operand. E.g.

foo <> bar|

will give you

foo <> bar <> |

or

foo <> |bar

will give you

foo <> | <> bar

This is more convenient than typing out the same operator."
  (interactive)
  (let* ((current-pair (shm-current-node-pair))
         (current (cdr current-pair))
         (parent-pair (shm-node-parent current-pair))
         (parent (cdr parent-pair)))
    (cond
     ((eq 'InfixApp (shm-node-cons parent))
      (let ((qop
             (or (shm-get-qop-string (cdr (shm-node-previous current-pair)))
                 (shm-get-qop-string (cdr (shm-node-next current-pair))))))
        (cond
         (qop
          (cond
           ((= (point) (shm-node-start current))
            (let ((point (point)))
              (insert " " qop " ")
              (goto-char point)))
           ((= (point) (shm-node-end current))
            (insert " " qop " "))
           (t (error "Please go to the start or end of the node to indicate direction."))))
         (t (error "Unable to figure out the operator.")))))
     (t (error "Not in an infix application.")))))

(defun shm/raise ()
  "Raise the expression up one, replacing its parent."
  (interactive)
  (let* ((current-pair (shm-current-node-pair))
         (current (cdr current-pair))
         (parent-pair (shm-node-parent current-pair (shm-node-type current)))
         (parent (cdr parent-pair)))
    (if parent
        (if (string= (shm-node-type current)
                     (shm-node-type parent))
            (let ((shm/raise-code (shm-kill-node 'buffer-substring-no-properties nil nil t)))
              (shm-kill-node 'buffer-substring-no-properties parent)
              (shm-insert-indented (lambda () (insert shm/raise-code)))
              (shm/reparse)))
      (error "No matching parent!"))))

;;; Killing and yanking

(defun shm/kill-region (beg end)
  "Kill the region, and save it in the clipboard."
  (interactive "r")
  (shm-kill-region nil beg end nil))

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
   (when (looking-at "[ ]+$")
     (delete-region (point) (line-end-position)))
   (cond
    ((and (shm-in-string)
          (not (= (point)
                  (shm-node-start (shm-current-node)))))
     (kill-region (point)
                  (1- (shm-node-end (shm-current-node)))))
    ((and (= (point) (line-end-position))
          (not (looking-at "\n[^ ]")))
     (let ((column (current-column)))
       (delete-region (point)
                      (save-excursion (forward-line 1)
                                      (goto-char (+ (line-beginning-position)
                                                    column))))
       (shm-kill-to-end-of-line t)))
    ((shm-current-node) (shm-kill-to-end-of-line))
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
   (when (looking-back "[a-zA-Z0-9]+_*")
     (shm-insert-string " "))
   (when (looking-at "[a-zA-Z0-9]+_*")
     (shm-insert-string " ")
     (forward-char -1))
   (shm-insert-indented #'clipboard-yank)))

(defun shm/yank-pop ()
  "Yank from the kill ring and insert indented with `shm-insert-indented'."
  (interactive)
  (shm-with-fallback
   yank-pop
   (if (not (eq last-command 'yank))
       (error "Previous command was not a yank (error from shm/yank-pop)"))
   (apply #'delete-region shm-last-yanked)
   (shm-insert-indented #'yank-pop)))

;;; Deletion

(defun shm/backward-kill-word ()
  "Kill the word backwards."
  (interactive)
  (let ((to-be-deleted (save-excursion (backward-word)
                                       (point))))
    (save-excursion
      (shm-appropriate-adjustment-point)
      (shm-adjust-dependents (point) (* -1 (- (point) to-be-deleted))))
    (backward-kill-word 1)))

(defun shm/delete ()
  "Delete the current node."
  (interactive)
  (let ((current (shm-current-node))
        (inhibit-read-only t))
    (delete-region (shm-node-start current)
                   (shm-node-end current))))

(defun shm/del ()
  "Character deletion handler.

Generally, we delete things in the current node. BUT, there are
some things that we shouldn't delete, because they would cause
parse errors that are rarely useful. For example:

    (|case x of _ -> _) -- where | indicates cursor.

"
  (interactive)
  (shm-with-fallback
   delete-backward-char
   (cond
    ;; These cases are “gliders”. They simply move over the character
    ;; backwards. These could be handled all as one regular
    ;; expression, but in the interest of clarity—for now—they are left
    ;; as separate cases.
    ((looking-back "[()]") (shm-delete-or-glide "(" ")"))
    ((looking-back "[[]") (shm-delete-or-glide "\\[" "\\]"))
    ((looking-back "[]]") (shm-delete-or-glide "\\[" "\\]"))
    ((looking-back "[{}]") (shm-delete-or-glide "{" "}"))
    ((looking-back "[\"]") (shm-delete-or-glide "\"" "\""))
    ((looking-back "[_ ]->") (forward-char -3))
    ((looking-back "[^A-Zaz0-9_]then[ ]*")
     (search-backward-regexp "[^ ][ ]*then")
     (unless (or (looking-at "$") (looking-at " "))
       (forward-char 1)))
    ((looking-back "[^A-Zaz0-9_]else[ ]*")
     (search-backward-regexp "[^ ][ ]*else")
     (unless (or (looking-at "$") (looking-at " "))
       (forward-char 1)))
    ((looking-back "[^A-Zaz0-9_]if[ ]*")
     nil) ; do nothing
    ((looking-back "[^A-Zaz0-9_]case[ ]*")
     nil) ; do nothing
    ((and (looking-at "= ")
          (looking-back " "))
     (forward-char -1))
    ((or (and (looking-back " = ")
              (not (looking-at "$"))
              (not (looking-at " ")))
         (and (looking-back "=")
              (looking-at " ")))
     (search-backward-regexp "[ ]+=[ ]*"
                             (line-beginning-position)
                             t
                             1)
     (when (looking-back " ")
       (when (search-backward-regexp "[^ ]" (line-beginning-position)
                                     t 1)
         (forward-char 1))))
    ;; This is the base case, we assume that we can freely delete
    ;; whatever we're looking back at, and that the node will be able
    ;; to re-parse it.
    (t (save-excursion
         (shm-appropriate-adjustment-point)
         (shm-adjust-dependents (point) -1))
       (shm-delete-char))))
  (shm/init t))


;; Overlays

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


;; Indentation

(defun shm-kill-node (&optional save-it node start do-not-delete)
  "Kill the current node.

See documentation of `shm-kill-region' for the transformations
this does."
  (interactive)
  (let* ((current (or node (shm-current-node))))
    (shm-kill-region save-it
                     (or start (shm-node-start current))
                     (shm-node-end current)
                     do-not-delete)))

(defun shm-kill-region (save-it start end do-not-delete)
  "Kill the given region, dropping any redundant indentation.

This normalizes everything it kills assuming what has been killed
is a node or set of nodes. Indentation is stripped off and
preserved appropriately so that if we kill e.g.

foo = {do bar
          mu}

where {} indicates the current node, then what is put into the kill ring is:

do bar
   mu

rather than what is normally put there,

do bar
          mu

So this is nice to paste elsewhere outside of Emacs, but it's
especially nice for pasting back into other parts of code,
because the yank function will take advantage of this
normalization and paste and re-indent to fit into the new
location. See `shm/yank' for documentation on that."
  (goto-char start)
  (let* ((start-col (current-column))
         (multi-line (/= (line-beginning-position)
                         (save-excursion (goto-char end)
                                         (line-beginning-position))))
         (string (buffer-substring-no-properties
                  start
                  end))
         (result
          (unless (string= string "")
            (with-current-buffer (get-buffer-create shm-kill-zone-name)
              (erase-buffer)
              (when multi-line
                (insert (make-string start-col ? )))
              (insert string)
              ;; This code de-indents code until a single line is hitting column zero.
              (while (progn (goto-char (point-min))
                            (not (and (search-forward-regexp "^[^ ]" nil t 1)
                                      (forward-line -1)
                                      ;; If there are empty lines, they
                                      ;; don't count as hitting column zero.
                                      (if (/= (line-beginning-position)
                                              (line-end-position))
                                          t
                                        ;; And we should actually delete empty lines.
                                        (progn (delete-region (1- (point)) (point))
                                               nil)))))
                ;; Bring everything back one.
                (indent-rigidly (point-min) (point-max)
                                -1))
              ;; If there's an empty line at the end, then strip that
              ;; out. It's just bothersome when pasting back in.
              (goto-char (point-max))
              (when (looking-at "^$")
                (delete-region (1- (point))
                               (point)))
              ;; Finally, the actual save.
              (funcall (if save-it save-it 'clipboard-kill-ring-save)
                       (point-min)
                       (point-max))))))
    (let ((inhibit-read-only t))
      (unless do-not-delete
        (delete-region start
                       end)))
    result))

(defun shm-appropriate-adjustment-point ()
  "Go to the appropriate adjustment point.

This is called before calling `shm-adjust-dependents', because some places, e.g.

zoo = do
  bar
  mu

If the point is at 'z', then we should *not* move 'bar' or 'mu',
even though we normally would. To avoid doing this, we use a very
simple but 90% effective (100% is rather hard, will not be
appearing in a beta version) heuristic. We jump to here:

zoo| = do
  bar
  mu

And use our normal adjustment test there. After all, only thing
after 'zoo' are *really* dependent."
  (let ((current (shm-current-node)))
    (when (and current
               (< (shm-node-end current) (line-end-position)))
      (goto-char (shm-node-end current)))))

(defun shm-newline-indent ()
  "Go to the next logical line from the current node at the right column.

This function uses the node's type to decode how to indent, and
in some cases will insert commas and things like for tuples and
lists."
  (let* ((current-pair (shm-current-node-pair))
         (current (cdr current-pair))
         (parent-pair (shm-node-parent current-pair))
         (parent (cdr parent-pair))
         (inhibit-read-only t))
    (cond
     ((or (string= (shm-node-type-name current)
                   "ImportSpecList")
          (and (string= (shm-node-type-name current)
                        "ModuleName")
               (looking-at "$")
               parent
               (string= (shm-node-type-name parent)
                        "ImportDecl")))
      (newline)
      (insert "import "))
     ;; When inside a list, indent to the list's position with an
     ;; auto-inserted comma.
     ((and parent
           (eq 'List (shm-node-cons parent)))
      (newline)
      (indent-to (shm-node-start-column parent))
      (insert ",")
      (shm-set-node-overlay parent-pair))
     ;; Lambdas indents k spaces inwards
     ((eq 'Lambda (shm-node-cons current))
      (newline)
      (indent-to (+ (shm-indent-spaces) (shm-node-start-column current))))
     ;; Indentation for function application.
     ((and parent
           (eq 'App (shm-node-cons parent)))
      (let ((column
             (save-excursion
               (shm/goto-parent)
               (forward-sexp)
               (1+ (current-column)))))
        (cond
         ((and (or (= column (current-column))
                   (= column (+ (shm-node-start-column parent)
                                (shm-indent-spaces))))
               (/= column (shm-node-start-column parent)))
          (newline)
          (indent-to (+ (shm-node-start-column parent)
                        (shm-indent-spaces))))
         (t
          (newline)
          (indent-to column)))))
     ;; Indent for sum types
     ((and parent
           (eq 'DataDecl (shm-node-cons parent)))
      (newline)
      (indent-to (shm-node-start-column current))
      (delete-char -2)
      (insert "| "))
     ;; Auto-insert commas for field updates
     ((or (string= "FieldUpdate" (shm-node-type-name current))
          (string= "FieldDecl" (shm-node-type-name current))
          (string= "ExportSpec" (shm-node-type-name current))
          (string= "ImportSpec" (shm-node-type-name current)))
      ;; This is hacky because HSE doesn't have special nodes for the
      ;; record and the update in record {update} and so we have to
      ;; figure out where the { starts. There is some additional
      ;; information in HSE's trees, but I haven't thought of a nice
      ;; way to extract that yet.
      (goto-char (shm-node-end parent))
      (backward-sexp)
      (let ((column (current-column)))
        (goto-char (shm-node-end current))
        (newline)
        (indent-to column)
        (insert ",")
        (insert (make-string (abs (- (shm-node-start-column current)
                                     (1+ column)))
                             ? ))
        (shm/init)))
     ((and parent
           (eq 'Lambda (shm-node-cons parent)))
      (cond
       ((eq shm-lambda-indent-style 'leftmost-parent)
        (let ((leftmost-parent (cdr (shm-find-furthest-parent-on-line parent-pair))))
          (newline)
          (indent-to (+ (shm-indent-spaces)
                        (shm-node-indent-column leftmost-parent)))))
       (t (newline)
          (indent-to (+ (shm-indent-spaces)
                        (shm-node-start-column parent))))))
     ;; Guards | foo = …
     ((or (string= "GuardedRhs" (shm-node-type-name current))
          (string= "GuardedAlt" (shm-node-type-name current)))
      (newline)
      (indent-to (shm-node-start-column current))
      (insert "| "))
     ;; Indent after or at the = (an rhs).
     ((and parent
           (or (string= "Rhs" (shm-node-type-name parent))
               (string= "Rhs" (shm-node-type-name current))
               (string= "GuardedAlt" (shm-node-type-name parent))
               (string= "GuardedRhs" (shm-node-type-name parent))))
      (newline)
      (indent-to (+ (shm-indent-spaces)
                    (shm-node-start-column (cdr (shm-node-parent parent-pair))))))
     ;; When in a field update.
     ((and parent
           (string= "FieldUpdate" (shm-node-type-name parent)))
      (newline)
      (indent-to (+ (shm-node-start-column parent)
                    (shm-indent-spaces))))
     ;; When in an alt list
     ((and parent
           (string= "GuardedAlts" (shm-node-type-name current)))
      (newline)
      (indent-to (+ (shm-node-start-column parent)
                    (shm-indent-spaces))))
     ;; When in a case alt.
     ((and parent
           (string= "GuardedAlts" (shm-node-type-name parent)))
      (newline)
      (let ((alt (cdr (shm-node-parent parent-pair))))
        (indent-to (+ (shm-node-start-column alt)
                      (shm-indent-spaces)))))
     ;; Copy infix operators similar to making new list/tuple
     ;; separators
     ((and parent
           (eq 'InfixApp (shm-node-cons parent)))
      (let* ((operand-pair (shm-node-previous current-pair))
             (operand (cdr operand-pair))
             (string (buffer-substring-no-properties (shm-node-start operand)
                                                     (shm-node-end operand))))
        (newline)
        (indent-to (shm-node-start-column operand))
        (insert string " ")))
     ;; Infix operators
     ((and parent
           (eq 'InfixApp (shm-node-cons parent)))
      (newline)
      (indent-to (+ (shm-node-start-column parent))))
     ;; Commenting out this behaviour for now
     ;; ((string= "Match" (shm-node-type-name current))
     ;;  (let ((name (cdr (shm-node-child-pair current-pair))))
     ;;    (newline)
     ;;    (indent-to (shm-node-start-column current))
     ;;    (insert (buffer-substring-no-properties (shm-node-start name)
     ;;                                            (shm-node-end name))
     ;;            " ")))
     ;; Default indentation just copies the current node's indentation
     ;; level. Generally works reliably, but has less than favourable
     ;; indentation sometimes. It just serves as a catch-all.
     (t
      (newline)
      (indent-to (shm-node-start-column current))))))

(defun shm-adjust-dependents (end-point n)
  "Adjust dependent lines by N characters that depend on this
line after END-POINT."
  (let ((line (line-number-at-pos)))
    (when (and (not (and (looking-back "^[ ]+")
                         (looking-at "[ ]*")))
               (save-excursion (goto-char end-point)
                               (forward-word)
                               (= (line-number-at-pos) line)))
      (shm-move-dependents n
                           end-point))))

(defun shm-move-dependents (n point)
  "Move dependent-with-respect-to POINT lines N characters forwards or backwards.

This is purely based on alignments. If anything is aligned after
the current column, then it's assumed to be a child of whatever
has recently changed at POINT, and thus we 'bring it along'
either forwards or backwards.

The algorithm isn't quite comprehensive, it needs special cases
for top-level functions and things like that."
  (save-excursion
    (let ((column (progn (goto-char point)
                         (current-column)))
          (point nil)
          (end-point nil))
      (while (and (= 0 (forward-line 1))
                  (or (not end-point)
                      (/= end-point (line-end-position))))
        (if (shm-line-indented-past (1+ column))
            (progn (unless point
                     (setq point (goto-char (line-beginning-position))))
                   (setq end-point (line-end-position)))
          (goto-char (point-max))))
      (when end-point
        (indent-rigidly point end-point n)))))

(defun shm-line-indented-past (n)
  "Is the current line indented past N?"
  (goto-char (line-beginning-position))
  (let ((column (search-forward-regexp "[^ ]" (line-end-position) t 1)))
    (if column
        (>= (1- (current-column)) n)
      t)))

(defun shm-insert-string (string)
  "Insert the given string."
  (save-excursion
    (shm-appropriate-adjustment-point)
    (shm-adjust-dependents (point) 1))
  (insert string)
  (shm/init t))

(defun shm-insert-indented (do-insert)
  "Insert, indented in The Right Way. Calls DO-INSERT to do the insertion.

This function assumes a certain semantic meaning towards the
contents of the kill ring. That is,

do bar
   mu

Is an expression which, when pasted, into

main =

should yield,

main = do bar
          mu

Which is so convenient it changes the way you work. However,
there is also the other case:

  do
bar
mu

This is what happens when you have expressions whose children
hang on the underside, and thus pasting these can be done in two
ways: (1) the above way, (2) or like this:

main = do
  bar
  mu

I.e. take the parent into account and try to re-paste an
underside dangling expression. I don't like this style. With SHM
this style becomes pointless and in fact detrimental. It's much
easier to read and manipulate children who are next to their
parents. But one must compromise and conform to some styles no
matter how poorly reasoned.

We can actually give the option for people to pick and choose
this underside dangling vs not. But that will be implemented as a
separate function rather than hard-coded into this one specific
operation."
  (let ((column (current-column))
        (line (line-beginning-position))
        (start (point)))
    (funcall do-insert)
    (when (= line (line-beginning-position))
      (shm-adjust-dependents start (- (current-column)
                                      column)))
    (let ((end (point)))
      (cond
       ((progn (goto-char start)
               (looking-at " "))
        (let ((node (cdr (shm-find-furthest-parent-on-line (shm-current-node-pair)))))
          (goto-char end)
          (indent-rigidly start
                          end
                          (+ (shm-indent-spaces)
                             (shm-node-indent-column node)))
          (delete-region start
                         (save-excursion
                           (goto-char start)
                           (search-forward-regexp "[ ]+" (line-end-position) t 1)))))
       (t (goto-char end)
          (indent-rigidly start end column)))
      (setq shm-last-yanked (list start (point)))
      (goto-char start))))

(defun shm-find-furthest-parent-on-line (current)
  "Find the parent which starts nearest to column 0 on the
current line.

This is used when indenting dangling expressions."
  (let ((parent (shm-node-parent current)))
    (if parent
        (if (= (line-beginning-position)
               (save-excursion (goto-char (shm-node-start (cdr parent)))
                               (line-beginning-position)))
            (shm-find-furthest-parent-on-line parent)
          current)
      current)))


;; Type information operations

(defun shm-present-type-info (node info)
  "Present type info to the user."
  (let ((info. (concat (shm-kill-node 'buffer-substring-no-properties node nil t)
                       " :: "
                       info)))
    (if shm-use-presentation-mode
        (if (fboundp 'haskell-present)
            (haskell-present "SHM-Node"
                             nil
                             info.)
          (message "%s" info))
      (message "%s" info))))

(defun shm-node-type-info (node)
  "Get the type of the given node."
  (shm-type-of-region (shm-node-start node)
                      (shm-node-end node)))

(defun shm-type-of-region (beg end)
  "Get a type for the region."
  (let ((types (shm-types-at-point beg)))
    (loop for type
          in types
          do (when (and (= (elt type 0) beg)
                        (= (elt type 1)
                           end))
               (return (elt type 2))))))

(defun shm-types-at-point (point)
  "Get a list of spans and types for the current point."
  (save-excursion
    (goto-char point)
    (let ((line (line-number-at-pos))
          (col (1+ (current-column)))
          (file-name (buffer-file-name)))
      (cond
       (shm-use-hdevtools
        (shm-parse-hdevtools-type-info
         (with-temp-buffer
           (call-process "hdevtools" nil t nil "type" "-g" "-fdefer-type-errors"
                         file-name
                         (number-to-string line)
                         (number-to-string col))
           (buffer-string))))))))

(defun shm-parse-hdevtools-type-info (string)
  "Parse type information from the output of hdevtools."
  (let ((lines (split-string string "\n+")))
    (loop for line
          in lines
          while (string-match "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \"\\(.+\\)\"$"
                              line)
          do (goto-char (point-min))
          collect
          (let ((start-line (string-to-number (match-string 1 line)))
                (end-line (string-to-number (match-string 3 line))))
            (vector (progn (forward-line (1- start-line))
                           (+ (line-beginning-position)
                              (1- (string-to-number (match-string 2 line)))))
                    (progn (when (/= start-line end-line)
                             (forward-line (1- (- start-line end-line))))
                           (+ (line-beginning-position)
                              (1- (string-to-number (match-string 4 line)))))
                    (match-string 5 line))))))


;; Internal buffer operations

(defun shm-get-qop-string (node)
  "Get the string of the operator, if the node is an operator."
  (when (string= (shm-node-type-name node) "QOp")
    (buffer-substring-no-properties (shm-node-start node)
                                    (shm-node-end node))))

(defun shm-kill-to-end-of-line (&optional prepend-newline)
  "Kill everything possible to kill after point before the end of
the line."
  (let* ((vector (shm-decl-ast))
         (current-pair (shm-current-node-pair))
         (current (cdr current-pair))
         (parent-pair (shm-node-ancestor-at-point current-pair (point)))
         (parent (cdr parent-pair)))
    (loop for i
          from 0
          to (length vector)
          until (or (>= i (length vector))
                    (let ((node (elt vector i)))
                      (and (>= (shm-node-start node)
                               (shm-node-start parent))
                           (<= (shm-node-end node)
                               (shm-node-end parent)))))
          finally (return
                   (let ((last-command (if prepend-newline 'kill-region last-command)))
                     (when prepend-newline
                       (kill-append "\n" nil))
                     (if (< i (length vector))
                         (shm-kill-node 'clipboard-kill-ring-save
                                        parent
                                        (point))
                       (let ((line-end-position (if prepend-newline
                                                    (save-excursion (forward-line)
                                                                    (line-end-position))
                                                  (line-end-position))))
                         (when (= (point)
                                  line-end-position)
                           (kill-region (point)
                                        line-end-position)))))))))

(defun shm-literal-insertion ()
  "Should a node have literal insertion?"
  (or (shm-in-string)
      (shm-in-comment)))

(defun shm-in-comment ()
  "Are we currently in a comment?"
  (or (eq 'font-lock-comment-delimiter-face
          (get-text-property (point) 'face))
      (eq 'font-lock-doc-face
          (get-text-property (point) 'face))
      (eq 'font-lock-comment-face
          (get-text-property (point) 'face))))

(defun shm-in-string ()
  "Are we in a string?"
  (or (eq 'font-lock-string-face
          (get-text-property (point) 'face))))

(defun shm-delimit (open close)
  "Insert the given delimiters.

This is a special function because it will do different things
depending on the context.

If we're in a string, it just inserts OPEN. If we're in an
expression, it will insert OPEN and CLOSE and put the point
between them. It will also space out so that there is space
between previous nodes and the next. E.g.

foo|(bar)

If you hit \" at | then you will get:

foo \"\" (bar)

It saves one having to type spaces; it's obvious what to do
here."
  (cond
   ((and (shm-literal-insertion)
         (not (string= open "\"")))
    (shm-insert-string open))
   (t
    (shm/reparse)
    (let ((current (shm-actual-node)))
      (cond
       ((shm-find-overlay 'shm-quarantine)
        (insert open)
        (let ((point (point)))
          (insert close)
          (goto-char point)))
       (t
        (shm-adjust-dependents
         (point)
         (+
          (if (not (looking-back "[ ,[({]"))
              (progn (insert " ") 1)
            0)
          (length open)
          (length close)))
        (insert open)
        (let ((point (point)))
          (insert close)
          (when (and (/= (point) (line-end-position))
                     (not (looking-at "[]){} ,]")))
            (insert " "))
          (goto-char point)
          (shm/init t))))))))

(defun shm-delete-or-glide (open close)
  "Delete the given OPEN/CLOSE delimiter, or simply glide over it
  if it isn't empty."
  (cond
   ;; If the delimiters are empty, we can delete the whole thing.
   ((shm-delimiter-empty open close)
    (let ((inhibit-read-only t))
      (shm-adjust-dependents (point) -2)
      (delete-region (1- (point))
                     (1+ (point)))))
   ;; If the delimiters aren't empty and we're in a literal, then go
   ;; ahead and elete the character.
   ((and (not (let ((current (shm-current-node)))
                (and current
                     (or (string= 'Lit (shm-node-cons current))
                         (string= 'PLit (shm-node-cons current)))
                     (looking-back open))))
         (shm-literal-insertion))
    (shm-delete-char))
   ;; Otherwise just glide over the character.
   (t
    (when (looking-back close)
      (forward-char -1)))))

(defun shm-delimiter-empty (open close)
  "Is the current expression delimited by OPEN and CLOSE empty?"
  (and (looking-back open)
       (looking-at close)))

(defun shm-delete-char ()
  "Delete a character backwards or delete the region, if there is
one active."
  (if (region-active-p)
      (delete-region (region-beginning)
                     (region-end))
    (delete-region (1- (point))
                   (point))))

(defun shm-decl-ast (&optional reparse)
  "Return the AST representing the current declaration at point.

If the AST has already been loaded, that is returned immediately,
otherwise it's regenerated. See the Internal AST section below
for more information."
  (let ((p (shm-decl-points)))
    (when p
      (shm-get-decl-ast (car p)
                        (cdr p)
                        reparse))))

(defun shm-current-node ()
  "Return just the current node, without its index.

See `shm-current-node-pair' for what 'current' means."
  (cdr (shm-current-node-pair)))

(defun shm-actual-node ()
  "Return just the actual current node, without its index.

Normally node functions only care about the current workable
node. This function will return the *actual* node at point. See
`shm-current-node-pair' for what 'workable' means."
  (cdr (shm-node-backwards)))

(defun shm-current-node-pair ()
  "Return the current workable node at point.

Workable means that it is something that we want to be able to
parse.

For example, if we're looking at a Name,

foobar

then that is all well and good, but we don't want to edit a Name,
nor a QName (the parent), we want to edit an Exp (parent-parent)
whose constructor will be a Var."
  (let ((current (shm-node-backwards)))
    (if current
        (if (and shm-current-node-overlay
                 (overlay-buffer shm-current-node-overlay)
                 (or (= (shm-node-start (cdr current))
                        (overlay-start shm-current-node-overlay))
                     (= (shm-node-end (cdr current))
                        (overlay-end shm-current-node-overlay))))
            (overlay-get shm-current-node-overlay 'node-pair)
          (shm-workable-node current))
      nil)))

(defun shm-current-workable-node ()
  "Returns the same as `shm-current-node' but including the index."
  (let ((current (shm-node-backwards)))
    (when current
      (shm-workable-node current))))

(defun shm-decl-node (start)
  "Get the top-level node of the declaration."
  (let* ((vector (save-excursion (goto-char start)
                                 (shm-decl-ast))))
    (elt vector 0)))

(defun shm-workable-node (current-pair)
  "Assume that the given CURRENT node is not workable, and look
at the parent. If the parent has the same start/end position,
then the parent is the correct one to work with."
  (let* ((parent-pair (shm-node-parent current-pair))
         (parent (cdr parent-pair))
         (current (cdr current-pair)))
    (if parent
        (if (and (= (shm-node-start current)
                    (shm-node-start parent))
                 (= (shm-node-end current)
                    (shm-node-end parent)))
            (if (string= (shm-node-type current) (shm-node-type parent))
                current-pair
              (shm-workable-node parent-pair))
          current-pair)
      current-pair)))

(defun shm-node-previous (node-pair)
  "Get the previous node of NODE-PAIR."
  (let ((vector (shm-decl-ast)))
    (loop for i
          downfrom (car node-pair)
          to -1
          until (or (= i -1)
                    (let ((node (elt vector i)))
                      (<= (shm-node-end node)
                          (shm-node-start (cdr node-pair)))))
          finally (return
                   (when (>= i 0)
                     (shm-workable-node (cons i
                                              (elt vector i))))))))

(defun shm-node-next (node-pair)
  "Get the next node of NODE-PAIR."
  (let ((vector (shm-decl-ast)))
    (loop for i
          from 0
          to (length vector)
          until (or (= i (length vector))
                    (let ((node (elt vector i)))
                      (>= (shm-node-start node)
                          (shm-node-end (cdr node-pair)))))
          finally (return
                   (when (< i (length vector))
                     (shm-workable-node (cons i
                                              (elt vector i))))))))

(defun shm-node-backwards (&optional start type bound)
  "Get the current node searching bottom up starting from START,
and optionally just searching for nodes of type TYPE. BOUND
restricts how far to look back.

This is the fundamental way to look for a node in the declaration
vector.

Backwards means we go from the last node in the list and go
backwards up the list, it doesn't mean backwards as in up the
tree."
  (let* ((vector (shm-decl-ast))
         (point (point)))
    (loop for i
          downfrom (if start
                       (max -1 start)
                     (1- (length vector)))
          to -1
          until (or (= i -1)
                    (let ((node (elt vector i)))
                      (or (and bound
                               (< (shm-node-start node)
                                  bound))
                          (and (>= point (shm-node-start node))
                               (<= point (shm-node-end node))
                               (or (not type)
                                   (string= type
                                            (shm-node-type node)))))))
          finally (return
                   (when (and (>= i 0)
                              (not (and bound
                                        (< (shm-node-start (elt vector i))
                                           bound))))
                     (cons i
                           (elt vector i)))))))

(defun shm-node-child-pair (node-pair)
  "Return the immediate child-pair of the given parent."
  (let ((vector (shm-decl-ast))
        (i (car node-pair)))
    (when (< i (1- (length vector)))
      (cons (1+ i)
            (elt vector (1+ i))))))

(defun shm-node-child (node-pair)
  "Return the immediate child of the given parent."
  (cdr (shm-node-child-pair node-pair)))

(defun shm-node-ancestor-at-point (node-pair point)
  "Find the highest up ancestor that still starts at this point."
  (let ((parent-pair (shm-node-parent node-pair)))
    (if parent-pair
        (if (= (shm-node-start (cdr parent-pair))
               point)
            (shm-node-ancestor-at-point parent-pair point)
          node-pair)
      node-pair)))

(defun shm-node-parent (node-pair &optional type bound)
  "Return the direct parent of the given node-pair.

The start and end point of the parent can be the same as the
child, and in fact is common."
  (save-excursion
    (goto-char (shm-node-start (cdr node-pair)))
    (let* ((actual-parent-pair (shm-node-backwards (1- (car node-pair))
                                                   type
                                                   bound))
           (maybe-parent-parent-pair (when (car actual-parent-pair)
                                       (shm-node-backwards (1- (car actual-parent-pair)))))
           (actual-parent (cdr actual-parent-pair))
           (maybe-parent-parent (cdr maybe-parent-parent-pair)))
      (cond ((and actual-parent-pair
                  maybe-parent-parent-pair
                  (string= (shm-node-type-name actual-parent)
                           (shm-node-type-name maybe-parent-parent))
                  (or (eq (shm-node-cons actual-parent) 'App)
                      (eq (shm-node-cons actual-parent) 'InfixApp))
                  (eq (shm-node-cons actual-parent)
                      (shm-node-cons maybe-parent-parent)))
             (shm-node-parent actual-parent-pair))
            (t actual-parent-pair)))))

(defun shm-decl-points ()
  "Get the start and end position of the current
declaration. This assumes that declarations start at column zero
and that the rest is always indented by one space afterwards, so
Template Haskell uses with it all being at column zero are not
expected to work."
  (save-excursion
    (let ((start (or (progn (goto-char (line-end-position))
                            (search-backward-regexp "^[^ \n]" nil t 1))
                     0))
          (end (progn (goto-char (1+ (point)))
                      (or (when (search-forward-regexp "[\n]+[^ \n]" nil t 1)
                            (forward-char -1)
                            (search-backward-regexp "[^\n ]" nil t)
                            (forward-char)
                            (point))
                          (point-max)))))
      (cons start end))))


;; Internal AST information acquisition functions

(defun shm-node-description (node)
  "Generate a description of the given node suitable to be put in
  the minibuffer. If no documentation can be found, it generates
  a reasonable string instead."
  (let* ((type-doc (assoc (shm-node-type-name node)
                          shm-ast-documentation))
         (con-doc (assoc (symbol-name (shm-node-cons node))
                         (cddr type-doc))))
    (if type-doc
        (format "Node type: “%s”: %s, case: %s\n%s"
                (nth 0 type-doc)
                (nth 1 type-doc)
                (if con-doc
                    (format "“%s”: %s"
                            (nth 0 con-doc)
                            (nth 1 con-doc))
                  (format "“%s” (no more info)"
                          (shm-node-cons node)))
                (shm-kill-node 'buffer-substring-no-properties
                               node
                               nil
                               t))
      (format "Node type: “%s” (no more info)"
              (shm-node-type-name node)))))

(defun shm-set-decl-ast (point ast)
  "Store the given decl AST at the given POINT. If there is
already an AST for a decl at the given point then remove that one
and instate this one."
  (setq shm-decl-asts
        (cons
         (cons (set-marker (make-marker) point) ast)
         (remove-if (lambda (pair)
                      (when (= (marker-position (car pair))
                               point)
                        (set-marker (car pair) nil)
                        t))
                    shm-decl-asts)))
  ast)

(defun shm-get-decl-ast (start end &optional reparse)
  "Get the AST of the declaration starting at POINT."
  (let ((pair (car (remove-if-not (lambda (pair)
                                    (= (marker-position (car pair))
                                       start))
                                  shm-decl-asts))))
    (if (and (not reparse)
             pair)
        (cdr pair)
      (progn
        (when (or (/= start shm-last-parse-start)
                  (/= end shm-last-parse-end))
          (setq shm-last-parse-start start)
          (setq shm-last-parse-end end)
          (let ((ast (shm-get-nodes (shm-get-ast "decl"
                                                 start
                                                 end)
                                    start
                                    end)))
            (if ast
                (progn (setq shm-lighter " SHM")
                       (when pair
                         (shm-delete-markers pair))
                       (shm-set-decl-ast start ast)
                       ;; Delete only quarantine overlays.
                       (shm-delete-overlays (point-min) (point-max) 'shm-quarantine)
                       (shm/init)
                       ast)
              (progn
                (when shm-display-quarantine
                  (shm-quarantine-overlay start end))
                (setq shm-lighter " SHM!")
                nil))))))))

(defun shm-delete-markers (decl)
  "Delete the markers in DECL."
  (mapc #'shm-node-delete-markers
        (cdr decl)))

(defun shm-get-ast (type start end)
  "Get the AST for the given region at START and END. Parses with TYPE.

This currently launches a fresh process and uses this buffer
nonsense, for any parse, which sucks, but is fast enough _right
now_. Later on a possibility to make this much faster is to have
a persistent running parser server and than just send requests to
it, that should bring down the roundtrip time significantly, I'd
imagine."
  (let ((message-log-max nil)
        (buffer (current-buffer)))
    (when (> end (1+ start))
      (with-temp-buffer
        (let ((temp-buffer (current-buffer)))
          (with-current-buffer buffer
            (condition-case e
                (call-process-region start
                                     end
                                     shm-program-name
                                     nil
                                     temp-buffer
                                     nil
                                     "parse"
                                     type)
              ((file-error)
               (error "Unable to find structured-haskell-mode executable! See README for help.")))))
        (read (buffer-string))))))

(defun shm-check-ast (type start end)
  "Check whether the region of TYPE from START to END parses.

This doesn't generate or return an AST, it just checks whether it
parses."
  (let ((message-log-max nil)
        (buffer (current-buffer)))
    (with-temp-buffer
      (let ((temp-buffer (current-buffer)))
        (with-current-buffer buffer
          (call-process-region start
                               end
                               shm-program-name
                               nil
                               temp-buffer
                               nil
                               "check"
                               ;; In other words, always parse with
                               ;; the more generic “decl” when
                               ;; something starts at column 0,
                               ;; because HSE distinguishes between a
                               ;; “declaration” and an import, a
                               ;; module declaration and a language
                               ;; pragma.
                               (if (save-excursion (goto-char start)
                                                   (= (point) (line-beginning-position)))
                                   "decl"
                                 type))))
      (string= "" (buffer-string)))))

(defun shm-get-nodes (ast start end)
  "Get the nodes of the given AST.

We convert all the line-col numbers to Emacs points and then
create markers out of them. We also store the type of the node,
e.g. Exp, and the case of the node, e.g. Lit or Case or Let,
which is helpful for doing node-specific operations like
indentation.

Any optimizations welcome."
  (let* ((start-end (cons start end))
         (start-column (save-excursion (goto-char start)
                                       (current-column))))
    (cond ((vectorp ast)
           (save-excursion
             (map 'vector
                  (lambda (node)
                    (vector
                     (elt node 0)
                     (elt node 1)
                     (progn (goto-char (car start-end))
                            (forward-line (1- (elt node 2)))
                            ;; This trick is to ensure that the first
                            ;; line's columns are offsetted for
                            ;; regions that don't start at column
                            ;; zero.
                            (goto-char (+ (if (= (elt node 2) 1)
                                              start-column
                                            0)
                                          (1- (+ (point) (elt node 3)))))
                            (let ((marker (set-marker (make-marker) (point))))
                                        ;(set-marker-insertion-type marker t)
                              marker))
                     (progn (goto-char (car start-end))
                            (forward-line (1- (elt node 4)))
                            ;; Same logic as commented above.
                            (goto-char (+ (if (= (elt node 4) 1)
                                              start-column
                                            0)
                                          (1- (+ (point) (elt node 5)))))
                            (let ((marker (set-marker (make-marker) (point))))
                              (set-marker-insertion-type marker t)
                              marker))))
                  ast)))
          (t nil))))


;; Node accessors

(defun shm-node-type (n)
  "Get the AST type of N."
  (elt n 0))

(defun shm-node-type-name (n)
  "Get just the constructor name part of N.

This doesn't always return the correct thing, e.g. [Foo Bar] will
return [Foo. It's just a convenience function to get things like
Case or whatnot"
  (nth 0 (split-string (elt n 0) " ")))

(defun shm-node-cons (n)
  "Get the constructor name of N."
  (elt n 1))

(defun shm-node-start (n)
  "Get the start position of N in its buffer."
  (marker-position (elt n 2)))

(defun shm-node-end (n)
  "Get the end position of N in its buffer."
  (marker-position (elt n 3)))

(defun shm-node-set-start (n x)
  "Set the start position of N."
  (set-marker (elt n 2) x))

(defun shm-node-set-end (n x)
  "Set the end position of N."
  (set-marker (elt n 3) x))

(defun shm-node-delete-markers (n)
  "Set the markers to NIL, which is about the best we can do for
deletion. The markers will be garbage collected eventually."
  (set-marker (elt n 2) nil)
  (set-marker (elt n 3) nil))

(defun shm-node-start-column (n)
  "Get the starting column of N."
  (save-excursion (goto-char (shm-node-start n))
                  (current-column)))

(defun shm-node-indent-column (n)
  "Get the starting column of N."
  (+ (shm-node-start-column n)
     (if (or (string= "Tuple" (shm-node-cons n))
             (string= "Paren" (shm-node-cons n))
             (string= "List" (shm-node-cons n)))
         1
       0)))

(defun shm-node-end-column (n)
  "Get the end column of N."
  (save-excursion (goto-char (shm-node-end n))
                  (current-column)))

(defun shm-node-empty (n)
  "Is the node empty of any text?"
  (= (shm-node-start n)
     (shm-node-end n)))

(defun shm-node-pp (n)
  "Pretty print the node."
  (format "%s: %S: %d—%d"
          (shm-node-type-name n)
          (shm-node-cons n)
          (shm-node-start n)
          (shm-node-end n)))


;; Provide

(provide 'shm)

;;; structured-haskell-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
