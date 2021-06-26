CP=cp

# Sphinx options.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
PAPER         =
BUILDDIR      = _build

PAPEROPT_a4     = -D latex_paper_size=a4
PAPEROPT_letter = -D latex_paper_size=letter
ALLSPHINXOPTS   = -d $(BUILDDIR)/doctrees -T -D language=en $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) .

all: ghdl.info

.PHONY: ghdl.texi

#---

man:
	$(SPHINXBUILD) -b man $(ALLSPHINXOPTS) $(BUILDDIR)/man

#---

html:
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) $(BUILDDIR)/html

#---

latex:
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) $(BUILDDIR)/latex

#---

texi: ghdl.texi
ghdl.texi:
	$(SPHINXBUILD) -b texinfo $(ALLSPHINXOPTS) $(BUILDDIR)/texinfo
	$(CP) $(BUILDDIR)/texinfo/GHDL.texi $@

info: ghdl.info
ghdl.info: ghdl.texi
	makeinfo -o $@ $<

dvi: ghdl.dvi
ghdl.dvi: ghdl.texi
	texi2dvi $<

ghdl.ps: ghdl.dvi
	dvips $<

pdf: ghdl.pdf
ghdl.pdf: ghdl.dvi
	dvipdf $<

#---

clean:
	$(RM) *~ *.dvi *.info ghdl.aux ghdl.cp ghdl.fn ghdl.ky ghdl.log
	$(RM) ghdl.pdf ghdl.pg ghdl.toc ghdl.tp ghdl.vr
	$(RM) -rf build

maintainer-clean: clean
	$(RM) ghdl.texi
