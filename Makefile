SHELL := /bin/bash
PACKAGE=mrgsolve.fu
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
CHKDIR=.

travis:
	make build
	R CMD check ${TARBALL} --as-cran

test:
	Rscript -e 'testthat::test_file("tests/testthat.R")'

rhub:
	Rscript -e 'rhub::check_for_cran()'

cran:
	make doc
	make build
	R CMD check ${TARBALL} --as-cran

covr:
	Rscript inst/covr/covr.R

all:
	make doc
	make build
	make install

.PHONY: doc
doc:
	Rscript -e 'devtool::document()'

.PHONY: build
build:
	R CMD build --md5 $(PKGDIR)

install:
	R CMD install --install-tests ${TARBALL}

check:
	make doc
	make build
	R CMD check ${TARBALL} -o ${CHKDIR}

readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'

site:
	Rscript -e 'pkgdown::build_site()'

spelling:
	Rscript inst/script/_spelling.R

check-win:
	Rscript -e 'devtools::check_win_devel()'
