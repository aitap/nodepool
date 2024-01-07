PACKAGE = $(shell Rscript -e "\
 cat(read.dcf('DESCRIPTION')[,c('Package','Version')], sep = '_'); \
 cat('.tar.gz') \
")
R = R

all: piks $(PACKAGE)

.PHONY: all piks repo check
.SUFFIXES: .pikchr .svg

PIKS = man/figures/architecture.pikchr
SVGS = $(PIKS:.pikchr=.svg)
piks: $(SVGS)
PIKCHR = pikchr
.pikchr.svg:
	$(PIKCHR) --svg-only $< > $@

$(PACKAGE): . R/* man/* man/*/* tests/* DESCRIPTION NAMESPACE .Rbuildignore README.md
	$(R) CMD build .

check: $(PACKAGE)
	$(R) CMD check $(PACKAGE)
