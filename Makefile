src        = ./src
emacs      = emacs
emacs_exec = $(emacs) --batch -nw -l init.el -f

emacs_version = $(shell $(emacs) -q --batch --exec \
		'(princ (format "%s.%s" emacs-major-version emacs-minor-version))')
emacs_ftp    = http://gnu.mirror.uber.com.au/emacs/emacs-$(emacs_version).tar.gz
emacs_gz      = $(src)/emacs-$(emacs_version).tar.gz
emacs_src_dir = $(src)/emacs-$(emacs_version)

# ----------------------------------------------------------------------------

default : conf elpa tags

all : $(emacs_src_dir) byte-compile-all tags

# Build tags file.
tags :; $(emacs_exec) 'cb:build-ctags'

# ----------------------------------------------------------------------------
# Byte-compilation tasks.

# Byte-compile files in ./lisp
conf :; $(emacs_exec) 'cb:byte-compile-conf'

# Byte-compile files in ./elpa
elpa :; $(emacs_exec) 'cb:byte-compile-elpa'

# Byte-compile all elisp files.
byte-compile-all : conf elpa tags

# ----------------------------------------------------------------------------
# Cleaning tasks.

# Perform all cleaning tasks.
clean : clean-elc clean-backups clean-flycheck

# Remove compiled elisp files.
clean-elc :
	rm -f *.elc
	rm -f lisp/*.elc
	rm -f lib/*.elc

# Remove backup files created outside backups directory.
clean-backups :
	rm -f *~
	rm -f lisp/*~
	rm -f lib/*~

# Remove temporary flycheck files.
clean-flycheck :
	rm -f flycheck-*
	rm -f lisp/flycheck-*
	rm -f lib/flycheck-*

# ----------------------------------------------------------------------------
# Fetching Emacs source.

# Download and extract the emacs source files for this emacs version.
emacs-source : $(emacs_src_dir)

# Download sources for this Emacs version to ./src
$(emacs_gz) :| $(src)
	curl $(emacs_ftp) -o $(emacs_gz)

# Perform expansion of Emacs source files.
$(emacs_src_dir) :| $(emacs_gz)
	tar xfz $(emacs_gz) --directory=$(src)

# Create source directory.
$(src) :;  mkdir $(src)
