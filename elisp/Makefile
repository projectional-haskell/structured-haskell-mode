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

test:
	@emacs -Q --batch shm-test.el --eval "(progn (add-to-list 'load-path \".\") (setq-default indent-tabs-mode nil) (transient-mark-mode 1) (require 'shm-test) (shm-test/run-all))"
