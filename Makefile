PACKAGE = $(shell Rscript -e "\
 cat(read.dcf('DESCRIPTION')[,c('Package','Version')], sep = '_'); \
 cat('.tar.gz') \
")

all: piks $(PACKAGE)

.PHONY: all piks repo check
.SUFFIXES: .pikchr .svg

PIKS = man/figures/architecture.pikchr
SVGS = $(PIKS:.pikchr=.svg)
piks: $(SVGS)
PIKCHR = pikchr
.pikchr.svg:
	$(PIKCHR) --svg-only $< > $@

$(PACKAGE): R/* man/* man/*/* DESCRIPTION NAMESPACE .Rbuildignore README.md
	R CMD build .

repo: PACKAGES.rds
PACKAGES.rds: PACKAGES.gz
PACKAGES.gz: PACKAGES
PACKAGES: $(PACKAGE)
	Rscript -e 'tools::write_PACKAGES(verbose = TRUE)'

check: $(PACKAGE)
	R CMD check $(PACKAGE)
