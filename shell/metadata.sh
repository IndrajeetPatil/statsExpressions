#!/bin/bash

# Update metadata for package
#
# To run on
#  - mac:     > sh <filename>.sh
#  - windows: > bash <filename>.sh

Rscript -e 'roxygen2::roxygenise()'
Rscript -e 'codemetar::write_codemeta()'
Rscript -e 'cffr::cff_write()'
