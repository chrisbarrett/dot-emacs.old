emacs = emacs

all:
	$(emacs) --batch -nw -f 'cb:byte-compile-lisp'

clean:
	rm *.elc
	rm lisp/*.elc
