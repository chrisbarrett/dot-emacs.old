EMACS ?= emacs
EMACSFLAGS = --batch -Q
CASK = cask
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)


SRCS = $(filter-out %-pkg.el, $(wildcard *.el))
OBJECTS = $(SRCS:.el=.elc)
DOC_SRCS = doc/__PROJECT-NAME__.org
INFO_MANUAL = doc/__PROJECT-NAME__.info
PACKAGE_SRCS = $(SRCS) __PROJECT-NAME__-pkg.el $(INFO_MANUAL)
PACKAGE = __PROJECT-NAME__-$(VERSION).tar

.PHONY: all
all : doc

.PHONY: compile
compile : $(OBJECTS)

.PHONY: test
test : compile
	$(CASK) exec ecukes

.PHONY: package
package : $(PACKAGE)

.PHONY: packages
packages : $(PKG_DIR)

$(PACKAGE) : $(PACKAGE_SRCS)
	rm -rf __PROJECT-NAME__-$(VERSION)
	mkdir -p __PROJECT-NAME__-$(VERSION)
	cp -f $(PACKAGE_SRCS) __PROJECT-NAME__-$(VERSION)
	tar cf $(PACKAGE) __PROJECT-NAME__-$(VERSION)
	rm -rf __PROJECT-NAME__-$(VERSION)

.PHONY: clean-all
clean-all : clean clean-info clean-elc clean-pkgdir

.PHONY: clean
clean :
	rm -f $(OBJECTS)
	rm -rf __PROJECT-NAME__-*.tar __PROJECT-NAME__-pkg.el

.PHONY: clean-pkgdir
clean-pkgdir :
	rm -rf $(PKG_DIR)

.PHONY: clean-info
clean-info :
	rm -f $(INFO_MANUAL)

.PHONY: clean-elc
clean-elc :
	rm -f ./*.elc

%.elc : %.el $(PKG_DIR)
	$(CASK) exec $(EMACS) $(EMACSFLAGS) \
	--eval '(setq package-user-dir "$(PKG_DIR)")' -f package-initialize \
	-f batch-byte-compile $<

.PHONY: doc
doc : $(INFO_MANUAL)

$(INFO_MANUAL) :
	$(EMACS) --eval \
	  '(progn \
		(require \'org)\
		(require \'ox-texinfo)\
		(find-file "$(DOC_SRCS)")\
		(org-export-to-file \'info "$(INFO_MANUAL)"))'

%.elc : %.el $(PKG_DIR)
	$(CASK) exec $(EMACS) -Q --batch $(EMACSFLAGS) \
	--eval '(setq package-user-dir "$(PKG_DIR)")' -f package-initialize \
	-f batch-byte-compile $<

$(PKG_DIR) : Cask
	$(CASK) install
	touch $(PKG_DIR)

__PROJECT-NAME__-pkg.el : Cask
	$(CASK) package
