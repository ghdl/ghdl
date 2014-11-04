ortho_srcdir=..
GNAT_FLAGS=-gnaty3befhkmr -gnata -gnatf -gnatwlcru -gnat05
CC=gcc
BE=mcode
SED=sed

all: $(ortho_exec)

$(ortho_exec): $(ortho_srcdir)/mcode/ortho_mcode.ads memsegs_c.o force
	gnatmake -m -o $@ -g -aI$(ortho_srcdir)/mcode -aI$(ortho_srcdir) \
	$(GNAT_FLAGS) ortho_code_main -bargs -E -largs memsegs_c.o #-static

memsegs_c.o: $(ortho_srcdir)/mcode/memsegs_c.c
	$(CC) -c $(CFLAGS) -o $@ $<

oread: force
	gnatmake -m -o $@ -g $(GNAT_FLAGS) -aI../oread ortho_code_main -aI.. -largs memsegs_c.o

elfdump: force
	gnatmake -m -g $(GNAT_FLAGS) $@

coffdump: force
	gnatmake -m $(GNAT_FLAGS) $@

clean:
	$(RM) -f *.o *.ali ortho_code_main elfdump
	$(RM) b~*.ad? *~

distclean: clean


force:

.PHONY: force all clean

ORTHO_BASENAME=ortho_mcode
include $(ortho_srcdir)/Makefile.inc
