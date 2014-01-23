ELFILES = \
	   shm-ast-documentation.el \
	   shm-evaporate.el \
	   shm.el \
	   shm-test.el \
	   shm-tests.el

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
