src        = $(CURDIR)/src
lib        = $(CURDIR)/lib
etc        = $(CURDIR)/etc
lisp       = $(CURDIR)/lisp
bin        = $(CURDIR)/bin
tmp        = $(CURDIR)/tmp
modules    = $(CURDIR)/.git/modules

emacs        ?= emacs
emacs_flags   = --batch -nw
emacs_version = $(shell $(emacs) -Q --batch --exec \
	  '(princ (format "%s.%s" emacs-major-version emacs-minor-version))')
emacs_src  = $(src)/emacs-$(emacs_version)

# ----------------------------------------------------------------------------

.PHONY: default langs tags \
	clean clean-elc clean-backups clean-flycheck clean-tmp \
	python jedi elpy pylint \
	supercollider \
	ruby rubocop growl \
	clang haskell

default : $(modules) $(emacs_src) tags

langs : ruby supercollider python clang haskell

tags :
	$(emacs) $(emacs_flags) -l package -l init.el -f build-ctags

$(etc) $(src) $(tmp) :
	mkdir $@

$(modules) :
	git submodule update --init

# ----------------------------------------------------------------------------
# Cleaning

# Perform langs cleaning tasks.
clean : clean-elc clean-backups clean-flycheck clean-tmp

# Remove compiled elisp files.
clean-elc :
	rm -f *.elc
	rm -f $(lisp)/*.elc
	rm -f $(lib)/*.elc

# Remove backup files.
clean-backups :
	rm -f ./*~
	rm -f $(lisp)/*~
	rm -f $(lib)/*~

# Remove temporary flycheck files.
clean-flycheck :
	rm -f flycheck-*
	rm -f $(lisp)/flycheck-*
	rm -f $(lib)/flycheck-*

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

python : jedi elpy pylint $(pymacs)

jedi :
	pip install virtualenv epc argparse jedi

elpy :
	pip install elpy rope pyflakes pep8

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

supercollider   :| $(sc_src) $(sc_ext)
	cp -r $(sc_src)/editors/scel/sc/* $(sc_ext)

# ----------------------------------------------------------------------------
# Ruby

rsense_version = 0.3
rsense     = $(bin)/rsense-$(rsense_version)/bin/rsense
rsense_url = http://cx4a.org/pub/rsense/rsense-$(rsense_version).tar.bz2
rsense_bz  = $(bin)/rsense-$(rsense_version).tar.bz2

ruby : $(rsense) rubocop growl

rubocop :
	gem install rubocop

growl :
	[[ `uname` == 'Darwin' ]] && gem install ruby_gntp

# RSense

$(rsense_bz) :
	curl $(rsense_url) -o $(rsense_bz)

$(rsense) :| $(rsense_bz)
	tar xvjf $(rsense_bz) --directory=$(bin)
	chmod a+x $(rsense)

clang :
	cd lib/clang-complete-async && make

# ----------------------------------------------------------------------------
# Haskell

haskell :
	cabal install hs-lint ghc-mod stylish-haskell structured-haskell-mode
