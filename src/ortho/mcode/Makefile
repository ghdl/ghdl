ortho_srcdir=..
GNATMAKE=gnatmake
GNATFLAGS=-gnaty3befhkmr -gnata -gnatf -gnatwlcru
CC=gcc
BE=mcode
SED=sed

all: $(ortho_exec)

$(ortho_exec): $(ortho_srcdir)/mcode/ortho_mcode.ads memsegs_c.o force
	$(GNATMAKE) -o $@ -g -aI$(ortho_srcdir)/mcode -aI$(ortho_srcdir) \
	-aI$(ortho_srcdir)/.. $(GNATFLAGS) -gnatw.A ortho_code_main \
	-bargs -E -largs $(LDFLAGS) memsegs_c.o #-static

memsegs_c.o: $(ortho_srcdir)/mcode/memsegs_c.c
	$(CC) -c $(CFLAGS) -o $@ $<

oread: $(ortho_srcdir)/mcode/ortho_mcode.ads force
	$(GNATMAKE) -m -o $@ -g $(GNATFLAGS) -aI../oread ortho_code_main -aI.. -largs $(LDFLAGS) memsegs_c.o

elfdump: force
	$(GNATMAKE) -m -g $(GNATFLAGS) $@

coffdump: force
	$(GNATMAKE) -m $(GNATFLAGS) $@

clean:
	$(RM) -f *.o *.ali ortho_code_main elfdump
	$(RM) b~*.ad? *~

distclean: clean


force:

.PHONY: force all clean

ORTHO_BASENAME=ortho_mcode
include $(ortho_srcdir)/Makefile.inc
