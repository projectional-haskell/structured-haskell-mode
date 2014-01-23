ELFILES = \
	   shm-ast-documentation.el \
	   shm-ast.el \
	   shm-customizations.el \
	   shm-deletion.el \
	   shm.el \
	   shm-evaporate.el \
	   shm-generic.el \
	   shm-globals.el \
	   shm-indentation.el \
	   shm-insertion.el \
	   shm-macros.el \
	   shm-navigation.el \
	   shm-nodes.el \
	   shm-overlays.el \
	   shm-skeletons.el \
	   shm-strings.el \
	   shm-test.el \
	   shm-tests.el \
	   shm-transform.el \
	   shm-types.el \
	   shm-yank-kill.el

ELCFILES = $(ELFILES:.el=.elc)

all: $(ELCFILES)

%.elc: %.el
	@emacs -Q -L . \
		--batch \
		--eval "(byte-compile-disable-warning 'cl-functions)" \
		-f batch-byte-compile \
		$<

clean:
	rm -f *.elc
