all: piks

.PHONY: all piks
.SUFFIXES: .pikchr .svg

PIKS = man/figures/architecture.pikchr
SVGS = $(PIKS:.pikchr=.svg)
piks: $(SVGS)
PIKCHR = pikchr
.pikchr.svg:
	$(PIKCHR) --svg-only $< > $@
