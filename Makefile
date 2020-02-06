# prepare the package for release
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)

all: bib build install clean

bib:
	cd inst/rmarkdown/resources;\
	./mendeleyBibFix

build:
	cd ..;\
	R CMD build --no-manual $(PKGNAME)

install:
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/;\
	$(RM) $(PKGNAME)_$(PKGVERS).tar.gz
