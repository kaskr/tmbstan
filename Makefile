R=R
# -> you can do    R=R-devel  make ....

PACKAGE=tmbstan
VERSION := $(shell sed -n '/^Version: /s///p' tmbstan/DESCRIPTION)
TARBALL := $(PACKAGE)_$(VERSION).tar.gz

install:
	R CMD INSTALL --preclean tmbstan

update-stan-model:
	R --slave < autogen.R

test:
	R --slave < test.R

doc-update: $(PACKAGE)/R/*.R
	echo "library(roxygen2);roxygenize(\"$(PACKAGE)\",roclets = c(\"namespace\"))" | $(R) --slave
	echo "library(roxygen2);roxygenize(\"$(PACKAGE)\",roclets = c(\"collate\", \"rd\"))" | $(R) --slave

build-package:
	$(R) CMD build --resave-data=no $(PACKAGE)

check:
	make doc-update
	make build-package
	$(R) CMD check $(TARBALL)

pdf:
	rm -f $(PACKAGE).pdf
	$(R) CMD Rd2pdf --no-preview $(PACKAGE)
