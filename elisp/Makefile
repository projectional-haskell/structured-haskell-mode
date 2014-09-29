ELFILES = shm-ast-documentation.el \
          shm-ast.el \
          shm-constraint.el \
          shm-customizations.el \
          shm-debug.el \
          shm-edit-string.el \
          shm.el \
          shm-evaporate.el \
          shm-indent.el \
          shm-in.el \
          shm-insert-del.el \
          shm-layout.el \
          shm-macros.el \
          shm-manipulation.el \
          shm-nav.el \
          shm-node.el \
          shm-overlays.el \
          shm-simple-indent.el \
          shm-slot.el \
          shm-test.el \
          shm-tests.el \
          shm-type.el \
          shm-yank-kill.el

EMACS = emacs
ELCFILES = $(ELFILES:.el=.elc)

all: $(ELCFILES)

%.elc: %.el
	@$(EMACS) -Q -L . \
		--batch \
		--eval "(byte-compile-disable-warning 'cl-functions)" \
		-f batch-byte-compile \
		$<

clean:
	rm -f *.elc

test:
	@$(EMACS) -Q --batch shm-test.el --eval "(progn (add-to-list 'load-path \".\") (setq-default indent-tabs-mode nil) (transient-mark-mode 1) (require 'shm-test) (shm-test/run-all))"

check:
	sh check.sh
