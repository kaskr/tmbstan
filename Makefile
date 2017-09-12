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
	R --vanilla < test.R

doc-update: $(PACKAGE)/R/*.R
	echo "library(roxygen2);roxygenize(\"$(PACKAGE)\",roclets = c(\"namespace\"))" | $(R) --slave
	echo "library(roxygen2);roxygenize(\"$(PACKAGE)\",roclets = c(\"collate\", \"rd\"))" | $(R) --slave

build:
	$(R) CMD build --resave-data=no $(PACKAGE)

check:
	$(R) CMD check --as-cran $(TARBALL)

pdf:
	rm -f $(PACKAGE).pdf
	$(R) CMD Rd2pdf --no-preview $(PACKAGE)
