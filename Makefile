src        = $(CURDIR)/src
backups    = $(CURDIR)/backups
lib        = $(CURDIR)/lib
etc        = $(CURDIR)/etc
lisp       = $(CURDIR)/lisp
bin        = $(CURDIR)/bin
tmp        = $(CURDIR)/tmp
modules    = $(CURDIR)/.git/modules
emacs      = emacs
emacs_src  = $(src)/emacs-$(emacs_version)

emacs_exec    = $(emacs) --batch -nw -l init.el -f
emacs_version = $(shell $(emacs) -Q --batch --exec \
	  '(princ (format "%s.%s" emacs-major-version emacs-minor-version))')

# ----------------------------------------------------------------------------

.PHONY: default
default : $(modules) tags $(emacs_src)

.PHONY: all
all : $(modules) $(emacs_src) tags \
	  ruby supercollider python clang haskell \

.PHONY: src
src : $(emacs_src)

.PHONY: tags
tags :
	$(emacs_exec) 'build-ctags'

# Directories

$(etc) :; mkdir $(etc)
$(src) :; mkdir $(src)
$(tmp) :; mkdir $(tmp)

$(modules) :
	git submodule update --init

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
	rm -rf $(tmp)/*

# ----------------------------------------------------------------------------
# Emacs source

emacs_ftp   = http://gnu.mirror.uber.com.au/emacs/emacs-$(emacs_version).tar.gz
emacs_gz    = $(src)/emacs-$(emacs_version).tar.gz

# Download and extract the emacs source files for this emacs version.

$(emacs_gz) :| $(src)
	curl $(emacs_ftp) -o $(emacs_gz)

$(emacs_src) :| $(emacs_gz)
	tar xvfz $(emacs_gz) --directory=$(src)

# ----------------------------------------------------------------------------
# Python

pymacs = $(lib)/Pymacs/pymacs.el

.PHONY: python
python : jedi elpy pylint $(pymacs)

.PHONY: jedi
jedi :
	pip install virtualenv epc argparse jedi

.PHONY: elpy
elpy :
	pip install elpy rope pyflakes pep8

.PHONY: pylint
pylint :
	pip install pylint

$(pymacs) :
	cd $(lib)/Pymacs && make

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
ruby : $(rsense) rubocop growl

.PHONY: rubocop
rubocop :
	gem install rubocop

.PHONY: growl
growl :
	[[ `uname` == 'Darwin' ]] && gem install ruby_gntp

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

.PHONY: haskell
haskell :
	cabal install happy alex ghci-ng ghc-mod \
		stylish-haskell structured-haskell-mode
