;;; shm.el --- Structured Haskell Mode

;; Copyright (c) 2013 Chris Done. All rights reserved.
;; Copyright (c) 1998 Heribert Schuetz, Graeme E Moss

;; Author:    Chris Done <chrisdone@gmail.com>
;; Created:   19-Oct-2013
;; Version:   1.0.0
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


;; Requirements

(require 'shm-ast-documentation)
(require 'shm-evaporate)

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
    (define-key map (kbd "-") 'shm/hyphen)
    (define-key map (kbd "#") 'shm/hash)
    (define-key map (kbd ",") 'shm/comma)
    (define-key map (kbd ":") 'shm/:)
    (define-key map (kbd "SPC") 'shm/space)
    (define-key map (kbd "C-c C-u") 'shm/insert-undefined)
    ;; Indentation
    (define-key map (kbd "C-j") 'shm/newline-indent)
    (define-key map (kbd "M-)") 'paredit-close-round-and-newline)
    (define-key map (kbd "C-c C-^") 'shm/swing-up)
    (define-key map (kbd "C-c C-j") 'shm/swing-down)
    (define-key map (kbd "TAB") 'shm/tab)
    (define-key map (kbd "<backtab>") 'shm/backtab)
    (define-key map (kbd "RET") 'shm/simple-indent-newline-same-col)
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
    map)
  "Structural editing operations keymap. Any key bindings in this
  map are intended to be only structural operations which operate
  with the tree in mind.")

;;;###autoload
(define-minor-mode structured-haskell-mode
  "Structured editing for Haskell."
  :lighter shm-lighter
  :keymap shm-map
  (if structured-haskell-mode
      (shm-mode-start)
    (shm-mode-stop)))


;; Modules

(require 'shm-generic)
(require 'shm-insertion)
(require 'shm-indentation)
(require 'shm-navigation)
(require 'shm-transform)
(require 'shm-yank-kill)
(require 'shm-strings)
(require 'shm-skeletons)
(require 'shm-types)
(require 'shm-ast)
(require 'shm-nodes)


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


;; Faces

(defface shm-quarantine-face
  '((((class color)) :background "#443333"))
  "Face for quarantines."
  :group 'shm)

(defface shm-current-face
  '((((class color)) :background "#373737"))
  "Face for the current node."
  :group 'shm)


;; Provide

(provide 'shm)

;;; shm.el ends here
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
