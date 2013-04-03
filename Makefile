emacs = emacs

conf:
	$(emacs) --batch -nw -l init.el -f 'cb:byte-compile-conf'

all:
	$(emacs) --batch -nw -l init.el -f 'cb:byte-compile-lisp'

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
	rm -f flycheck-*-
	rm -f lisp/flycheck-*-
	rm -f lib/flycheck-*-
