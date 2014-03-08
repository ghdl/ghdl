ortho_srcdir=..
GNAT_FLAGS=-gnaty3befhkmr -gnata -gnatf -gnatwael
CC=clang
LLVM_CONFIG=llvm-config
SED=sed
BE=llvm

all: $(ortho_exec)

$(ortho_exec): $(ortho_srcdir)/llvm/ortho_llvm.ads force llvm-cbindings.o
	gnatmake -m -o $@ -g -aI$(ortho_srcdir)/llvm -aI$(ortho_srcdir) \
	$(GNAT_FLAGS) ortho_code_main -bargs -E \
	-largs llvm-cbindings.o `$(LLVM_CONFIG) --ldflags --libs --system-libs` -lc++ #-static

llvm-cbindings.o: $(ortho_srcdir)/llvm/llvm-cbindings.cpp
	$(CC) -c  -I`$(LLVM_CONFIG) --includedir --cflags` -g -o $@ $<

clean:
	$(RM) -f *.o *.ali ortho_code_main
	$(RM) b~*.ad? *~

distclean: clean


force:

.PHONY: force all clean

ORTHO_BASENAME=ortho_llvm
include $(ortho_srcdir)/Makefile.inc
