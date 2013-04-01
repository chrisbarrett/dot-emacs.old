emacs = emacs

all:
	$(emacs) --batch -nw -l init.el -f 'cb:byte-compile-lisp'

clean:
	rm -f *.elc
	rm -f lisp/*.elc
