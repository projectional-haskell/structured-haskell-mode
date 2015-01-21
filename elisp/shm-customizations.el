;;; shm-customizations.el --- Structured Haskell Mode

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


;; Group

(defgroup shm nil
  "Structured editing mode for Haskell"
  :group 'haskell)


;; Faces

(defface shm-quarantine-face
  '((((class color)) :background "#443333"))
  "Face for quarantines."
  :group 'shm)

(defface shm-current-face
  '((((class color)) :background "#373737"))
  "Face for the current node."
  :group 'shm)


;; Customizations

(defcustom shm-auto-insert-skeletons
  t
  "Auto-insert skeletons for case, if, etc."
  :group 'shm
  :type 'boolean)

(defcustom shm-auto-insert-bangs
  t
  "Auto-insert bangs when inserting :: in record fields."
  :group 'shm
  :type 'boolean)

(defcustom shm-skip-applications
  t
  "Skip successive applications to the top parent.

So if you have

foo| bar mu

And go up a parent, it will go to

foo bar mu|

instead of

foo bar| mu

I tend to want the former behaviour more often than the latter,
but others may differ."
  :group 'shm
  :type 'boolean)

(defcustom shm-program-name
  "structured-haskell-mode"
  "The path to call for parsing Haskell syntax."
  :group 'shm
  :type 'string)

(defcustom shm-indent-spaces
  (if (boundp 'haskell-indent-spaces)
      haskell-indent-spaces
    2)
  "The number of spaces to indent by default."
  :group 'shm
  :type 'string)

(defcustom shm-language-extensions
  (if (boundp 'haskell-language-extensions)
      haskell-language-extensions
    '())
  "Language extensions in use. Should be in format: -XFoo, -XNoFoo etc."
  :group 'shm
  :type '(repeat 'string))

(defcustom shm-lambda-indent-style
  nil
  "Specify a particular style for indenting lambdas?"
  :group 'shm
  :type '(choice (const leftmost-parent) (const nil)))

(defcustom shm-use-presentation-mode
  nil
  "Use haskell-presentation-mode?"
  :group 'shm
  :type 'boolean)

(defcustom shm-display-quarantine
  t
  "Display quarantine?"
  :group 'shm
  :type 'boolean)

(defcustom shm-use-hdevtools
  nil
  "Use hdevtools for type information?"
  :group 'shm
  :type 'boolean)

(defcustom shm-type-info-fallback-to-ghci
  t
  "Fallback to GHCi when the type-info backend returns nothing?"
  :group 'shm
  :type 'boolean)

(defcustom shm-colon-enabled
  nil
  "Do special insertion of colons."
  :group 'shm
  :type 'boolean)

(defcustom shm-prevent-parent-deletion
  t
  "Prevent backspacing over parent heads that would break the
syntax."
  :group 'shm
  :type 'boolean)

(defcustom shm-idle-timeout
  0.2
  "Number of seconds before re-parsing."
  :group 'shm
  :type 'string)

(defcustom shm-indent-point-after-adding-where-clause
  nil
  "Whether to indent point to the next line when inseting where clause, e.g.
| being a point:

foo x = ...
  where
    |

when option is t, as opposed to

foo x = ...
  where |

when option is nil.
"
  :group 'shm
  :type 'boolean)

(defcustom shm-pragmas
  '("LANGUAGE" "OPTIONS_GHC" "INCLUDE" "DEPRECATED" "WARNING"
    "INLINE" "NOINLINE" "INLINABLE" "CONLIKE" "LINE" "RULES"
    "SPECIALIZE" "UNPACK" "SOURCE" "SCC")
  "Pragmas supported."
  :group 'shm
  :type 'list)


;; Provide

(provide 'shm-customizations)

;;; shm.el ends here
;; End:
