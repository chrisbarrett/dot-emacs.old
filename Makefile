src        = ./src
backups    = ./backups
lib        = ./lib
lisp       = ./lisp
emacs      = emacs
emacs_exec = $(emacs) --batch -nw -l init.el -f

emacs_version = $(shell $(emacs) -Q --batch --exec \
		'(princ (format "%s.%s" emacs-major-version emacs-minor-version))')
emacs_ftp    = http://gnu.mirror.uber.com.au/emacs/emacs-$(emacs_version).tar.gz
emacs_gz      = $(src)/emacs-$(emacs_version).tar.gz
emacs_src_dir = $(src)/emacs-$(emacs_version)

# ----------------------------------------------------------------------------

default : conf elpa tags

all : emacs-source byte-compile-all tags

# Build tags file.
tags :; $(emacs_exec) 'cb:build-ctags'

# ----------------------------------------------------------------------------
# Byte-compilation

# Byte-compile files in ./lisp
conf :; $(emacs_exec) 'cb:byte-compile-conf'

# Byte-compile files in ./elpa
elpa :; $(emacs_exec) 'cb:byte-compile-elpa'

# Byte-compile all elisp files.
byte-compile-all : conf elpa tags

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

# Download and extract the emacs source files for this emacs version.
emacs-source : $(src)
	curl $(emacs_ftp) -o $(emacs_gz)
	tar xfz $(emacs_gz) --directory=$(src)

# Create source directory.
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
supercollider   : $(sc_src) $(sc_ext) ; cp -r $(sc_src)/editors/scel/sc/* $(sc_ext)
$(sc_ext)       :; mkdir -p $(sc_ext)
$(sc_src)       :; git clone $(sc_github) $(sc_src)
