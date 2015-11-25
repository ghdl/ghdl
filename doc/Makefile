SRC_FILES= \
 Copyrights.rst \
 Flaws_and_bugs_report.rst \
 GHDL_implementation_of_VHDL.rst \
 GHDL_implementation_of_VITAL.rst \
 Introduction.rst \
 Invoking_GHDL.rst \
 Simulation_and_runtime.rst \
 Starting_with_GHDL.rst \
 index.rst \
 conf.py

CP=cp

# Sphinx options.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
PAPER         =
BUILDDIR      = build

PAPEROPT_a4     = -D latex_paper_size=a4
PAPEROPT_letter = -D latex_paper_size=letter
ALLSPHINXOPTS   = -d $(BUILDDIR)/doctrees $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) .

all: ghdl.info

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

html:
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) $(BUILDDIR)/html

ghdl.texi: $(SRC_FILES)
	$(SPHINXBUILD) -b texinfo $(ALLSPHINXOPTS) $(BUILDDIR)/texinfo
	$(CP) $(BUILDDIR)/texinfo/GHDL.texi $@

clean:
	$(RM) *~ *.dvi *.info ghdl.aux ghdl.cp ghdl.fn ghdl.ky ghdl.log
	$(RM) ghdl.pdf ghdl.pg ghdl.toc ghdl.tp ghdl.vr
	$(RM) -rf build

maintainer-clean: clean
	$(RM) ghdl.texi
