BE=debug
ortho_srcdir=..

orthobe_srcdir=$(ortho_srcdir)/$(BE)

GNATMAKE=gnatmake
CC=gcc
CFLAGS=-g
ALL_GNAT_FLAGS=-pipe -g -gnato -gnatwl -gnatf -gnaty3befhkmr -gnatwu
GNATMAKE_FLAGS=-m $(ALL_GNAT_FLAGS) $(GNAT_FLAGS) -aI$(ortho_srcdir) -aI$(orthobe_srcdir) -aI.
#LARGS=-largs -static
SED=sed

all: $(ortho_exec)


$(ortho_exec): force $(orthobe_srcdir)/ortho_debug.ads
	gnatmake -o $@ $(GNATMAKE_FLAGS) ortho_debug-main -bargs -E $(LARGS)

clean:
	$(RM) -f *.o *.ali *~ b~*.ad? ortho_nodes-main
	$(RM) $(orthobe_srcdir)/ortho_debug.ads

force:

ORTHO_PACKAGE=Ortho_Debug
ORTHO_BASENAME=$(orthobe_srcdir)/ortho_debug

include $(ortho_srcdir)/Makefile.inc