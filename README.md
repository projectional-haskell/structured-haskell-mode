# structured-haskell-mode

This minor mode provides structured editing operations based on the syntax
of Haskell. In short-hand it's called SHM and throughout the codebase,
too. It acts a bit like, and is heavily inspired by,
[paredit-mode](https://www.youtube.com/watch?v=D6h5dFyyUX0) for Emacs.

![http://i.imgur.com/MBP9sdz.gif](http://i.imgur.com/MBP9sdz.gif)

In using structured-haskell-mode you will find that your layout style
will change and become more regular as the editor does the menial work
for you. Given that, some assumptions about style are made in
structured-haskell-mode and are best described by
[this style guide](https://github.com/chrisdone/haskell-style-guide).

[![Build Status](https://travis-ci.org/chrisdone/structured-haskell-mode.png)](https://travis-ci.org/chrisdone/structured-haskell-mode) [![Hackage](https://img.shields.io/hackage/v/structured-haskell-mode.svg?style=flat)](https://hackage.haskell.org/package/structured-haskell-mode)

## Features

Its features work by parsing the current declaration with an
executable called `structured-haskell-mode`, and then creates marks
for all the nodes' positions in the buffer.

In paredit-mode, manipulation of the tree is so enjoyable because all
the boundaries of nodes are explicitly specified by parentheses. Not
so, in Haskell. To get around this limitation, we have a “current”
node, which is always highlighted with a background color. With that
in place, one is able to do all of the operations that paredit can do.

Feature | Explanation
--- | --- | ---
![newline-indent](http://chrisdone.com/structured-haskell-mode/gifs-nocache/newline-indent.gif) | **Indenting**: `shm/newline-indent` (`C-j`) takes the current node and its type into consideration giving very predictable and useful behaviour.
![goto-parent](http://chrisdone.com/structured-haskell-mode/gifs-nocache/goto-parent.gif) | **Going to parent**: `shm/goto-parent` (`M-a`) jumps to the start of the parent.
![goto-parent-end](http://chrisdone.com/structured-haskell-mode/gifs-nocache/goto-parent-end.gif) | **Going to parent end**: `shm/goto-parent-end` (`)`) jumps to the end of the parent.
![add-list-item](http://chrisdone.com/structured-haskell-mode/gifs-nocache/add-list-item.gif) | **Adding a list item**: `shm/newline-indent` (`C-j`) will automatically add a comma when inside a list.
![add-operand](http://chrisdone.com/structured-haskell-mode/gifs-nocache/add-operands.gif) | **Adding operands**: `shm/add-operand` (`C-+`) will look at the current node and add another operand in the direction the cursor is leaning towards.
![auto-re-indent](http://chrisdone.com/structured-haskell-mode/gifs-nocache/auto-re-indent.gif) | **Auto-reindenting**: Typing and deleting will automatically re-indent dependent code blocks.
![raise](http://chrisdone.com/structured-haskell-mode/gifs-nocache/raise.gif) | **Raising**: `shm/raise` (`M-r`) raises the current node to replace its parent. If its direct parent is not the same expression type, it continues up the tree.
![re-indenting](http://chrisdone.com/structured-haskell-mode/gifs-nocache/re-indenting.gif) | **Re-indenting**: `shm/newline-indent` (`C-j`) and `shm/delete-indentation` (`M-^`) allow you to bring nodes inwards or outwards relative to the parent.
![record-syntax](http://chrisdone.com/structured-haskell-mode/gifs-nocache/record-syntax.gif) | **Record syntax**: Creating new elements with record syntax, like lists (and tuples) automatically adds the right separators. Note: you have to use `)` to expand the current node to be `foo :: X` rather than merely `X` in order to use `C-j` to get a new record field.
![kill-yank](http://chrisdone.com/structured-haskell-mode/gifs-nocache/kill-yank.gif) | **Copy/pasting**: `shm/kill` (`M-k`) and `shm/yank` (`C-y`) take indentation into account, and automatically normalize so that re-inserting will indent properly.
![kill-lines](http://chrisdone.com/structured-haskell-mode/gifs-nocache/kill-multiple-line.gif) | **Killing lines**: `shm/kill-line` (`C-k`) and `shm/yank` (`C-y`) also take indentation into account for killing and pasting, working with multiple lines at once happily.
![skeletons](http://chrisdone.com/structured-haskell-mode/gifs-nocache/skeletons.gif) | **Skeletons**: Typing prefixes of common syntax will auto-fill in the structure with "slots" that you can hit `TAB` to jump to, which auto-disappear when you type in them.
![imports](http://chrisdone.com/structured-haskell-mode/gifs-nocache/imports.gif) | **Imports**: Imports have some limited support when using `C-j` to auto-create new import lines, and `C-c C-q` to qualify/unqualify.
![multi-strings](http://chrisdone.com/structured-haskell-mode/gifs-nocache/multi-strings.gif) | **Multi-line strings**: Use `C-j` inside a string to split it into multiple lines. Use backspace at the start of a line to join it with the previous.
![pragmas](http://chrisdone.com/structured-haskell-mode/gifs-nocache/pragmas.gif) | **Pragmas**: There is some limited support to help with typing pragmas `{-# … #-}`.
![case-split](http://chrisdone.com/structured-haskell-mode/gifs-nocache/case-split.gif) | **Case split**: If you are using `interactive-haskell-mode`, you can get case splits for simple sum types, you can `(require 'shm-case-split)` make a keybinding for `shm/case-split`.

Useful keybinding for case-split.

``` lisp
(define-key shm-map (kbd "C-c C-s") 'shm/case-split)
```

See `shm.el` for other keybindings. You might want to disable or
change some of the bindings to suit your tastes.

## How to enable

Clone the project:

    $ git clone https://github.com/chrisdone/structured-haskell-mode.git

You need to install the structured-haskell-mode executable which does
the parsing.

    $ cd structured-haskell-mode
    $ cabal install
    $ cd elisp/
    $ make

Add the elisp library to your `load-path` and require the library.

    (add-to-list 'load-path "/path/to/structured-haskell-mode/elisp")
    (require 'shm)

Then add it to your haskell-mode-hook:

    (add-hook 'haskell-mode-hook 'structured-haskell-mode)

Turn off
[haskell-indentation-modes](https://github.com/haskell/haskell-mode/wiki/Indentation#basic-indentation). They
are incompatible with structured-haskell-mode. It has its own
indentation functionality.

You'll want to customize these two variables: `shm-quarantine-face`
and `shm-current-face` to something that better suites your color
theme.

**Solarized-light users**

The following are apparently pretty good for solarized-light.

    (set-face-background 'shm-current-face "#eee8d5")
    (set-face-background 'shm-quarantine-face "lemonchiffon")

## Checking it works

Some users have trouble with the executable being in their PATH
properly. That's fine, here's how to check that you're setup.

1. Open a Haskell file and go to a _syntactically valid_ declaration,
   e.g. `main = return ()`.
2. Check that your modeline contains `SHM` only.

    * `SHM!` means a parse error.
    * `SHM?` means it hasn't been able to parse _anything_ yet.

   Both in this scenario should not appear, if they do, see the
   next steps.

If your modeline is not `SHM` and the current declaration doesn't have
a grey box anywhere in it, then you have a problem.

Go back to the declaration and try `M-x shm/test-exe`. You should be
taken to a `*shm-scratch-test*` buffer containing a vector of source
spans. If you don't, and you have something more like "program not
found", then you need to make sure it's findable.

You can try:

1. Set the Emacs `PATH`:

        (setenv "PATH" (shell-command-to-string "echo $PATH"))

2. Set the binary path that SHM calls:

        (setq shm-program-name "/absolute/path/to/structured-haskell-mode")

3. Get the `exec-path-from-shell` package
   [here](https://github.com/purcell/exec-path-from-shell.git) and try that.

After that, disable and re-enable `structured-haskell-mode`.

## Development

Byte-compiling:

    make clean
    make check
    make

### Run tests

You can run the tests with the following:

    (require 'shm-test')
    M-x shm-test/run-all

Or with make:

    make clean
    make test

### Write tests

To write tests there's a script for making them. Run `M-x
shm-test/new` and follow the instructions that look something like
this:

    -- Steps to create a test
    --
    --  1. Insert the test-case setup code.
    --  2. Move the cursor to the starting point.
    --  3. Hit C-c C-c to record cursor position.
    --  4. Press F3 to begin recording the test actions.
    --  5. Do the action.
    --  6. Hit F4 to complete the action and run C-c C-c.

Then copy the resulting elisp to shm-tests.el and run the tests to
check it works properly.

## FAQ

### What does it use to parse?

It uses haskell-src-exts to parse code. It could just as easily use
GHC as a backend, but from benchmarks, GHC is only twice as fast. When
it's the difference between 15ms and 30ms for a 400 line module, it
really does not matter. We're parsing declarations and individual
nodes. Plus the GHC tree is more annoying to traverse generically due
to its partiality.

### How do I disable some keybindings?

You can disable any keybinding in the structured-haskell-mode map by
defining the key as `nil`:

``` lisp
(define-key shm-map (kbd "M-{") nil)
```

## Reporting a bug

***Note:*** If you get a parse error (e.g. via `M-x shm/test-exe`) for
   valid code that is using fairly new (read: couple years) a GHC
   extension, you are probably hitting the fact that
   [HSE](https://github.com/haskell-suite/haskell-src-exts/issues/19)
   doesn't parse a bunch of newer GHC extensions. SHM _does not do any
   parsing itself_, it
   [uses HSE](https://github.com/chrisdone/structured-haskell-mode/blob/master/src/Main.hs). There
   are some patches in the HSE repo, provided as pull requests, which
   you can try applying to a local copy of HSE and then recompile SHM
   with the new version.

To get extra useful information, always run:

    M-: (setq debug-on-error t)

And then re-run the same thing that gave you the problem. It will give
you a backtrace that you can paste into the issue.

When reporting a bug, please write in the following format:

    [Any general summary/comments if desired]

    Steps to reproduce:

        Type blah in the buffer.
        Hit x key.
        See some change z.
        Hit y key.

    Expected:

    What I expected to see and happen.

    Actual:

    What actually happened.
