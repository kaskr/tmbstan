R=R
# -> you can do    R=R-devel  make ....

PACKAGE=tmbstan
VERSION := $(shell sed -n '/^Version: /s///p' tmbstan/DESCRIPTION)
DATE := $(shell sed -n '/^Date: /s///p' tmbstan/DESCRIPTION)
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

cran-version:
	cd tmbstan; git clean -xdf
	#rm -rf tmbstan/tests
	#mkdir -p tmbstan/tests
	#cp test.R tmbstan/tests
	make doc-update
	make build

## Get a rough changelog since most recent github revision tag
## (Use as starting point when updating NEWS file)
## NOTE: Run *after* updating version and date in DESCRIPTION.
changelog:
	echo; \
	echo "------------------------------------------------------------------------"; \
	echo tmbstan $(VERSION) \($(DATE)\); \
	echo "------------------------------------------------------------------------"; \
	echo; \
	git --no-pager log --format="o %B" `git describe --abbrev=0 --tags`..HEAD | sed s/^-/\ \ -/g
