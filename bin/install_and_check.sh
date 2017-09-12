#!/bin/bash
set -e # Exit with nonzero exit code if anything fails

make build
make check
make install
