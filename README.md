# structured-haskell-mode

This minor mode provides structured editing operations based on the syntax
of Haskell. In short-hand it's called SHM and throughout the codebase,
too. It acts a bit like, and is heavily inspired by,
[paredit-mode](http://www.emacswiki.org/emacs/ParEdit) for Emacs.

## Features

Its features work by parsing the current declaration with an
executable called `structured-haskell-mode`, and then creates marks
for all the nodes' positions in the buffer.

Feature | Explanation
--- | --- | ---
![newline-indent](http://chrisdone.com/structured-haskell-mode/gifs/newline-indent.gif) | **Indenting**: `shm/newline-indent` takes the current node and its type into consideration giving very predictable and useful behaviour.
![goto-parent](http://chrisdone.com/structured-haskell-mode/gifs/goto-parent.gif) | **Going to parent**: `shm/goto-parent` jumps to the start of the parent.
![goto-parent-end](http://chrisdone.com/structured-haskell-mode/gifs/goto-parent-end.gif) | **Going to parent end**: `shm/goto-parent-end` jumps to the end of the parent.
![add-list-item](http://chrisdone.com/structured-haskell-mode/gifs/add-list-item.gif) | **Adding a list item**: `shm/newline-indent` (`C-j`) will automatically add a comma when inside a list.
![add-operand](http://chrisdone.com/structured-haskell-mode/gifs/add-operand.gif) | **Adding operands**: `shm/add-operand` (`C-+`) will look at the current node and add another operand in the direction the cursor is leaning towards.
![auto-re-indent](http://chrisdone.com/structured-haskell-mode/gifs/auto-re-indent.gif) | **Auto-reindenting**: Typing and deleting will automatically re-indent dependent code blocks.
![raise](http://chrisdone.com/structured-haskell-mode/gifs/raise.gif) | **Raising**: `shm/raise` raises the current node to replace its parent. If its direct parent is not the same expression type, it continues up the tree.
![re-indenting](http://chrisdone.com/structured-haskell-mode/gifs/re-indenting.gif) | **Re-indenting**: `shm/newline-indent` and `shm/delete-indentation` allow you to bring nodes inwards or outwards relative to the parent.
![record-syntax](http://chrisdone.com/structured-haskell-mode/gifs/record-syntax.gif) | **Record syntax**: Creating new elements with record syntax, like lists (and tuples) automatically adds the right separators.
![transposition](http://chrisdone.com/structured-haskell-mode/gifs/transposition.gif) | **Transposition**: `shm/tranpose` will swap two sibling nodes.
![kill-yank](http://chrisdone.com/structured-haskell-mode/gifs/kill-yank.gif) | **Copy/pasting**: `shm/kill` and `shm/yank` take indentation into account, and automatically normalize so that re-inserting will indent properly.
![kill-lines](http://chrisdone.com/structured-haskell-mode/gifs/kill-multiple-line.gif) | **Killing lines**: `shm/kill-line` and `shm/yank` also take indentation into account for killing and pasting, working with multiple lines at once happily.

## How to enable

Clone the project:

    $ git clone https://github.com/chrisdone/structured-haskell-mode.git

You need to install the structured-haskell-mode executable which does
the parsing.

    $ cd structured-haskell-mode
    $ cabal install

Add the elisp library to your `load-path` and require the library.

    (add-to-list 'load-path "/path/to/structured-haskell-mode/elisp")
    (require 'structured-haskell-mode)

Then add it to your haskell-mode-hook:

    (add-hook 'haskell-mode-hook 'structured-haskell-mode)

## Development

### Run tests

You can run the tests with the following command line:

    emacs -l shm-ast-documentation.el \
          -l shm.el \
          -l shm-test.el \
          -l shm-tests.el \
          --eval '(shm-test/run-all)' \
          --debug

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
