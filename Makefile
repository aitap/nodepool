PACKAGE = $(shell Rscript -e "\
 cat(read.dcf('DESCRIPTION')[,c('Package','Version')], sep = '_'); \
 cat('.tar.gz') \
")
R = R

all:

.PHONY: all piks repo check
.SUFFIXES: .pikchr .svg

PIKS = man/figures/architecture.pikchr
SVGS = $(PIKS:.pikchr=.svg)
PIKCHR = pikchr
.pikchr.svg:
	$(PIKCHR) --svg-only $< > $@
all: $(SVGS)

$(PACKAGE): . R/* man/* man/*/* tests/* DESCRIPTION NAMESPACE .Rbuildignore README.md
	$(R) CMD build .
all: $(PACKAGE)

check: $(PACKAGE)
	$(R) CMD check $(PACKAGE)
