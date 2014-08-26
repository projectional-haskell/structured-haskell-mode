;;; shm.el --- Structured Haskell Mode

;; Copyright (c) 2013 Chris Done. All rights reserved.
;; Copyright (c) 1998 Heribert Schuetz, Graeme E Moss

;; Author:    Chris Done <chrisdone@gmail.com>
;; Created:   19-Oct-2013
;; Version:   1.0.2
;; Keywords:  development, haskell, structured
;; Stability: unstable

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

;; A minor mode for adding structured editing to Haskell.

;;; Code:

(require 'shm-edit-string)
(require 'shm-constraint)
(require 'shm-type)
(require 'shm-simple-indent)
(require 'shm-yank-kill)
(require 'shm-slot)
(require 'shm-indent)
(require 'shm-insert-del)
(require 'shm-nav)
(require 'shm-manipulation)
(require 'shm-debug)

(defvar shm-map
  (let ((map (make-sparse-keymap)))
    ;; Insertion
    (define-key map (kbd "\"") 'shm/double-quote)
    (define-key map (kbd "(") 'shm/open-paren)
    (define-key map (kbd "M-(") 'shm/wrap-parens)
    (define-key map (kbd "[") 'shm/open-bracket)
    (define-key map (kbd "{") 'shm/open-brace)
    (define-key map (kbd "-") 'shm/hyphen)
    (define-key map (kbd "#") 'shm/hash)
    (define-key map (kbd ",") 'shm/comma)
    (define-key map (kbd ":") 'shm/:)
    (define-key map (kbd "SPC") 'shm/space)
    (define-key map (kbd "C-c C-u") 'shm/insert-undefined)
    (define-key map (kbd "C-c C-_") 'shm/insert-underscore)
    (define-key map (kbd "M-;") 'shm/comment)
    (define-key map (kbd "C-c C-e") 'shm/export)
    (define-key map (kbd "C-M-o") 'shm/split-line)
    ;; Indentation
    (define-key map (kbd "C-j") 'shm/newline-indent-proxy)
    (define-key map (kbd "M-)") 'paredit-close-round-and-newline)
    (define-key map (kbd "C-c C-^") 'shm/swing-up)
    (define-key map (kbd "C-c C-j") 'shm/swing-down)
    (define-key map (kbd "TAB") 'shm/tab)
    (define-key map (kbd "<backtab>") 'shm/backtab)
    (define-key map (kbd "RET") 'shm/ret-proxy)
    (define-key map (kbd "C-<return>") 'shm/simple-indent-newline-indent)
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
    (define-key map (kbd "M-w") 'shm/copy-region)
    (define-key map (kbd "C-M-k") 'shm/kill-node)
    (define-key map (kbd "C-y") 'shm/yank)
    (define-key map (kbd "M-y") 'shm/yank-pop)
    ;; Navigation
    (define-key map (kbd "C-M-f") 'shm/forward-node)
    (define-key map (kbd "C-M-b") 'shm/backward-node)
    (define-key map (kbd "M-a") 'shm/goto-parent)
    (define-key map (kbd ")") 'shm/close-paren)
    (define-key map (kbd "]") 'shm/close-bracket)
    (define-key map (kbd "}") 'shm/close-brace)
    (define-key map (kbd "M-}") 'shm/forward-paragraph)
    (define-key map (kbd "M-{") 'shm/backward-paragraph)
    (define-key map (kbd "C-M-SPC") 'shm/mark-node)
    (define-key map (kbd "C-c C-w") 'shm/goto-where)
    ;; Splitting, slurping, barfing, etc.
    (define-key map (kbd "C-+") 'shm/add-operand)
    (define-key map (kbd "M-r") 'shm/raise)
    (define-key map (kbd "M-s") 'shm/splice)
    (define-key map (kbd "C-c C-q") 'shm/qualify-import)
    map)
  "Structural editing operations keymap. Any key bindings in this
  map are intended to be only structural operations which operate
  with the tree in mind.")

(defvar shm-parsing-timer nil
  "The timer used to re-parse every so often. The idle time can
  be configured with `shm-idle-timeout'.")

;;;###autoload
(define-minor-mode structured-haskell-mode
  "Structured editing for Haskell."
  :lighter shm-lighter
  :keymap shm-map
  (if structured-haskell-mode
      (shm-mode-start)
    (shm-mode-stop)))

(defvar shm-repl-map
  (let ((map (make-sparse-keymap)))
    ;; Insertion
    (define-key map (kbd "\"") 'shm/double-quote)
    (define-key map (kbd "(") 'shm/open-paren)
    (define-key map (kbd "M-(") 'shm/wrap-parens)
    (define-key map (kbd "[") 'shm/open-bracket)
    (define-key map (kbd "{") 'shm/open-brace)
    (define-key map (kbd "-") 'shm/hyphen)
    (define-key map (kbd "#") 'shm/hash)
    (define-key map (kbd ",") 'shm/comma)
    (define-key map (kbd ":") 'shm/:)
    (define-key map (kbd "SPC") 'shm/space)
    (define-key map (kbd "C-c C-u") 'shm/insert-undefined)
    (define-key map (kbd "C-c C-_") 'shm/insert-underscore)
    (define-key map (kbd "M-;") 'shm/comment)
    ;; Navigation
    (define-key map (kbd "C-M-f") 'shm/forward-node)
    (define-key map (kbd "C-M-b") 'shm/backward-node)
    (define-key map (kbd "M-a") 'shm/goto-parent)
    (define-key map (kbd ")") 'shm/close-paren)
    (define-key map (kbd "]") 'shm/close-bracket)
    (define-key map (kbd "}") 'shm/close-brace)
    (define-key map (kbd "M-}") 'shm/forward-paragraph)
    (define-key map (kbd "M-{") 'shm/backward-paragraph)
    (define-key map (kbd "C-M-SPC") 'shm/mark-node)
    (define-key map (kbd "TAB") 'shm/tab)
    (define-key map (kbd "<backtab>") 'shm/backtab)
    ;; Killing / yanking
    (define-key map (kbd "C-k") 'shm/kill-line)
    (define-key map (kbd "M-k") 'shm/kill-node)
    (define-key map (kbd "C-w") 'shm/kill-region)
    (define-key map (kbd "M-w") 'shm/copy-region)
    (define-key map (kbd "C-M-k") 'shm/kill-node)
    (define-key map (kbd "C-y") 'shm/yank)
    (define-key map (kbd "M-y") 'shm/yank-pop)
    ;; Deletion
    (define-key map (kbd "DEL") 'shm/del)
    (define-key map (kbd "<deletechar>") 'shm/delete)
    (define-key map (kbd "M-^") 'shm/delete-indentation)
    (define-key map (kbd "M-DEL") 'shm/backward-kill-word)
    (define-key map (kbd "C-<backspace>") 'shm/backward-kill-word)
    ;; Splitting, slurping, barfing, etc.
    (define-key map (kbd "C-+") 'shm/add-operand)
    (define-key map (kbd "M-r") 'shm/raise)
    (define-key map (kbd "M-s") 'shm/splice)
    (define-key map (kbd "C-c C-q") 'shm/qualify-import)
    map)
  "Structural editing operations keymap for in the REPL. This
  differs to `shm-map' by having keybindings more appropriate for
  a REPL, with inappropriate ones removed.")

(define-minor-mode structured-haskell-repl-mode
  "Structured editing for Haskell inside a REPL."
  :lighter shm-lighter
  :keymap shm-repl-map
  (cond
   ((eq major-mode 'haskell-interactive-mode)
    (if structured-haskell-repl-mode
        (shm-mode-start)
      (shm-mode-stop)))
   (t (structured-haskell-repl-mode -1)
      (error "Unsupported REPL mode: %S" major-mode))))

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

(defun shm-mode-stop ()
  "Stop the minor mode. Restore various settings and clean up any
state that will hopefully be garbage collected."
  ;; Kill the timer.
  (cancel-timer shm-parsing-timer)
  (setq shm-parsing-timer nil)
  ;; Kill self-insert hooks.
  (remove-hook 'post-self-insert-hook 'shm-post-self-insert t)
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
  (when (or structured-haskell-mode
            structured-haskell-repl-mode)
    (shm/reparse)))

(defun shm/tab ()
  "Either indent if at the start of a line, or jump to the next
  slot."
  (interactive)
  (cond
   ((save-excursion (goto-char (line-beginning-position))
                    (looking-at "^[ ]*$"))
    (shm/simple-indent))
   (t
    (shm/jump-to-slot))))

(defun shm/backtab ()
  "Either de-indent if at the start of a line, or jump to the previous
  slot."
  (interactive)
  (cond
   ((save-excursion (goto-char (line-beginning-position))
                    (looking-at "^[ ]*$"))
    (shm/simple-indent-backtab))
   (t
    (shm/jump-to-previous-slot))))

(defun shm/ret-proxy ()
  "Run `shm/simple-indent-newline-same-col', or in electric mode
  run `shm/newline-indent' (swaps behaviour)."
  (interactive)
  (if (bound-and-true-p electric-indent-mode)
      (call-interactively 'shm/newline-indent)
    (call-interactively 'shm/simple-indent-newline-same-col)))

(defun shm/newline-indent-proxy ()
  "Run `shm/newline-indent', or in electric mode
  run `simple-indent-newline-same-col' (swaps behaviour)."
  (interactive)
  (if (bound-and-true-p electric-indent-mode)
      (call-interactively 'shm/simple-indent-newline-same-col)
    (call-interactively 'shm/newline-indent)))

(provide 'shm)
