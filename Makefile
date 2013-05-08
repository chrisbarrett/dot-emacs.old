src        = ./src
backups    = ./backups
lib        = ./lib
etc        = ./etc
lisp       = ./lisp
bin        = ./bin
tmp        = ./tmp
emacs      = emacs

emacs_exec    = $(emacs) --batch -nw -l init.el -f
emacs_version = $(shell $(emacs) -Q --batch --exec \
	  '(princ (format "%s.%s" emacs-major-version emacs-minor-version))')

# ----------------------------------------------------------------------------

.PHONY: default
default : conf tags

.PHONY: all
all : $(emacs_src_d) compile tags ruby supercollider python clang scheme

# Build tags file.
.PHONY: tags
tags :
	$(emacs_exec) 'cb:build-ctags'

# Directories

$(etc) :; mkdir $(etc)
$(src) :; mkdir $(src)
$(tmp) :; mkdir $(tmp)

# ----------------------------------------------------------------------------
# Byte-compilation

# Byte-compile files in ./lisp
.PHONY: conf
conf :
	$(emacs_exec) 'cb:byte-compile-conf'

# Byte-compile files in ./elpa
.PHONY: elpa
elpa :
	$(emacs_exec) 'cb:byte-compile-elpa'

# Byte-compile all elisp files.
.PHONY: compile
compile : conf elpa tags

# ----------------------------------------------------------------------------
# Cleaning

# Perform all cleaning tasks.
.PHONY: clean
clean : clean-elc clean-backups clean-flycheck clean-tmp

# Remove compiled elisp files.
.PHONY: clean-elc
clean-elc :
	rm -f *.elc
	rm -f $(lisp)/*.elc
	rm -f $(lib)/*.elc

# Remove backup files.
.PHONY: clean-backups
clean-backups :
	rm -f ./*~
	rm -f $(lisp)/*~
	rm -f $(lib)/*~

# Remove temporary flycheck files.
.PHONY: clean-flycheck
clean-flycheck :
	rm -f flycheck-*
	rm -f $(lisp)/flycheck-*
	rm -f $(lib)/flycheck-*

.PHONY: clean-tmp
clean-tmp :
	rm -f $(tmp)/*

# ----------------------------------------------------------------------------
# Emacs source

emacs_src_d = $(src)/emacs-$(emacs_version)
emacs_ftp   = http://gnu.mirror.uber.com.au/emacs/emacs-$(emacs_version).tar.gz
emacs_gz    = $(src)/emacs-$(emacs_version).tar.gz

# Download and extract the emacs source files for this emacs version.

$(emacs_gz) :| $(src)
	curl $(emacs_ftp) -o $(emacs_gz)

$(emacs_src_d) :| $(emacs_gz)
	tar xvfz $(emacs_gz) --directory=$(src)

# ----------------------------------------------------------------------------
# Python

.PHONY: python
python : jedi elpy

.PHONY: jedi
jedi :
	pip install virtualenv epc argparse jedi

.PHONY: elpy
elpy :
	pip install elpy rope pyflakes pep8

# ----------------------------------------------------------------------------
# SuperCollider

sc_app_support = ~/Library/Application\ Support/SuperCollider
sc_ext         = $(sc_app_support)/Extensions
sc_github      = git://github.com/supercollider/supercollider.git
sc_src         = ~/src/SuperCollider

# Install SuperCollider emacs extensions.

$(sc_ext)       :; mkdir -p $(sc_ext)
$(sc_src)       :; git clone $(sc_github) $(sc_src)

.PHONY: supercollider
supercollider   :| $(sc_src) $(sc_ext)
	cp -r $(sc_src)/editors/scel/sc/* $(sc_ext)

# ----------------------------------------------------------------------------
# Ruby

rsense_version = 0.3
rsense     = $(bin)/rsense-$(rsense_version)/bin/rsense
rsense_url = http://cx4a.org/pub/rsense/rsense-$(rsense_version).tar.bz2
rsense_bz  = $(bin)/rsense-$(rsense_version).tar.bz2

.PHONY: ruby
ruby : $(rsense) rubocop

.PHONY: rubocop
rubocop :
	sudo gem install rubocop

# RSense

$(rsense_bz) :
	curl $(rsense_url) -o $(rsense_bz)

$(rsense) :| $(rsense_bz)
	tar xvjf $(rsense_bz) --directory=$(bin)
	chmod a+x $(rsense)

# ----------------------------------------------------------------------------

.PHONY: clang
clang :
	cd lib/clang-complete-async && make

# ----------------------------------------------------------------------------

r5rs_html    = $(etc)/r5rs-html
r5rs_gz      = $(tmp)/r5rs-html.tar.gz
r5rs_url     = http://www.schemers.org/Documents/Standards/R5RS/r5rs-html.tar.gz

.PHONY: scheme
scheme : $(etc) $(r5rs_html)

# Download Scheme documentation.

$(r5rs_html) :| $(r5rs_gz) $(etc)
	tar xfzv $(r5rs_gz) --directory=$(tmp)
	mv $(tmp)/html $(r5rs_html)

$(r5rs_gz) :| $(tmp)
	curl $(r5rs_url) -o $(r5rs_gz)
