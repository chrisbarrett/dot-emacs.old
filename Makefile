src        = ./src
backups    = ./backups
lib        = ./lib
lisp       = ./lisp
bin        = ./bin
emacs      = emacs

emacs_exec    = $(emacs) --batch -nw -l init.el -f
emacs_version = $(shell $(emacs) -Q --batch --exec \
      '(princ (format "%s.%s" emacs-major-version emacs-minor-version))')

# ----------------------------------------------------------------------------

default : conf elpa tags

all : $(emacs_src_d) compile tags ruby supercollider python

# Build tags file.
tags :; $(emacs_exec) 'cb:build-ctags'

# ----------------------------------------------------------------------------
# Byte-compilation

# Byte-compile files in ./lisp
conf :; $(emacs_exec) 'cb:byte-compile-conf'

# Byte-compile files in ./elpa
elpa :; $(emacs_exec) 'cb:byte-compile-elpa'

# Byte-compile all elisp files.
compile : conf elpa tags

# ----------------------------------------------------------------------------
# Cleaning

# Perform all cleaning tasks.
clean : clean-elc clean-backups clean-flycheck

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

# ----------------------------------------------------------------------------
# Emacs source

emacs_src_d = $(src)/emacs-$(emacs_version)
emacs_ftp   = http://gnu.mirror.uber.com.au/emacs/emacs-$(emacs_version).tar.gz
emacs_gz    = $(src)/emacs-$(emacs_version).tar.gz

# Download and extract the emacs source files for this emacs version.

$(emacs_gz) : $(src)
	curl $(emacs_ftp) -o $(emacs_gz)

$(emacs_src_d) : $(emacs_gz)
	tar xvfz $(emacs_gz) --directory=$(src)

$(src) :;  mkdir $(src)

# ----------------------------------------------------------------------------
# Python

python : jedi

# Install Jedi for python auto-completion.

jedi       : virtualenv epc argparse ; pip install jedi
epc        :; pip install epc
virtualenv :; pip install virtualenv
argparse   :; pip install argparse

# ----------------------------------------------------------------------------
# SuperCollider

sc_app_support = ~/Library/Application\ Support/SuperCollider
sc_ext         = $(sc_app_support)/Extensions
sc_github      = git://github.com/supercollider/supercollider.git
sc_src         = ~/src/SuperCollider

# Install SuperCollider emacs extensions.

$(sc_ext)       :; mkdir -p $(sc_ext)
$(sc_src)       :; git clone $(sc_github) $(sc_src)

supercollider   : $(sc_src) $(sc_ext)
	cp -r $(sc_src)/editors/scel/sc/* $(sc_ext)

# ----------------------------------------------------------------------------
# Ruby

ruby : $(rsense) rubocop

# Install rsense.

rsense_version = 0.3
rsense     = $(bin)/rsense-$(rsense_version)/bin/rsense
rsense_url = http://cx4a.org/pub/rsense/rsense-$(rsense_version).tar.bz2
rsense_bz  = $(bin)/rsense-$(rsense_version).tar.bz2

$(rsense_bz) :; curl $(rsense_url) -o $(rsense_bz)

$(rsense) : $(rsense_bz) ;
	tar xvjf $(rsense_bz) --directory=$(bin)
	chmod a+x $(rsense)

# Install rubocop

rubocop :; sudo gem install rubocop
