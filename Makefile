emacs      = emacs
emacs_exec = $(emacs) --batch -nw -l init.el -f

# ----------------------------------------------------------------------------

.PHONY: conf elpa tags all clean clean-elc clean-backups

all: conf elpa tags

conf:
	$(emacs_exec) 'cb:byte-compile-conf'

elpa:
	$(emacs_exec) 'cb:byte-compile-elpa'

tags:
	$(emacs_exec) 'build-ctags'


clean: clean-elc clean-backups clean-flycheck

clean-elc:
	rm -f *.elc
	rm -f lisp/*.elc
	rm -f lib/*.elc

clean-backups:
	rm -f *~
	rm -f lisp/*~
	rm -f lib/*~

clean-flycheck:
	rm -f flycheck-*
	rm -f lisp/flycheck-*
	rm -f lib/flycheck-*
