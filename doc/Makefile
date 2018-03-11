SRCS= \
conf.py ../src/version.in \
prolog.inc shields.inc shieldswho.inc \
about.rst \
appendix/Meta.rst appendix/Roadmap.rst \
building/Building.rst \
building/Directories.rst \
building/gcc/GNULinux-GNAT.rst \
building/gcc/index.rst \
building/gcc/Windows-MinGW-GNAT.rst \
building/llvm/GNULinux-GNAT.rst \
building/llvm/index.rst \
building/llvm/Windows-MinGW-GNAT.rst \
building/mcode/GNULinux-GNAT.rst \
building/mcode/index.rst \
building/mcode/Windows-GNATGPL.rst \
building/mcode/Windows-MinGW-GNAT.rst \
building/PrecompileVendorPrimitives.rst \
contribute.rst \
genindex.rst \
getting/Releases.rst \
index.rst \
licenses.rst \
references/CodingStyle.rst \
references/CommandReference.rst \
references/ImplementationOfVHDL.rst \
references/ImplementationOfVITAL.rst \
using/InvokingGHDL.rst \
using/QuickStartGuide.rst \
using/Simulation.rst

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

texi: ghdl.texi
ghdl.texi: $(SRCS)
	$(SPHINXBUILD) -b texinfo $(ALLSPHINXOPTS) $(BUILDDIR)/texinfo
	$(CP) $(BUILDDIR)/texinfo/GHDL.texi $@

clean:
	$(RM) *~ *.dvi *.info ghdl.aux ghdl.cp ghdl.fn ghdl.ky ghdl.log
	$(RM) ghdl.pdf ghdl.pg ghdl.toc ghdl.tp ghdl.vr
	$(RM) -rf build

maintainer-clean: clean
	$(RM) ghdl.texi
