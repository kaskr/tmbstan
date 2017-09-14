#!/bin/bash
set -e # Exit with nonzero exit code if anything fails

Rscript -e 'pkg <- c("rstan", "TMB"); if(!all(pkg%in%installed.packages()))install.packages(pkg)'

git clone https://github.com/kaskr/adcomp
cd adcomp; make install
