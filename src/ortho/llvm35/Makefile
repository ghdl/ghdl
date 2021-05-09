ortho_srcdir=..
GNATFLAGS=-gnaty3befhkmr -gnata -gnatf -gnatwael
CXX=clang++
LLVM_CONFIG=llvm-config
GNATMAKE=gnatmake
SED=sed
BE=llvm35

all: $(ortho_exec)

$(ortho_exec): $(ortho_srcdir)/llvm35/ortho_llvm.ads force llvm-cbindings.o
	$(GNATMAKE) -o $@ -aI$(ortho_srcdir)/llvm -aI$(ortho_srcdir) \
	$(GNATFLAGS) ortho_code_main -bargs -E \
	-largs llvm-cbindings.o --LINK=$(CXX) \
	`$(LLVM_CONFIG) --ldflags --libs --system-libs` $(LDFLAGS)

llvm-cbindings.o: $(ortho_srcdir)/llvm35/llvm-cbindings.cpp
	$(CXX) -c `$(LLVM_CONFIG) --cxxflags` -o $@ $<

clean:
	$(RM) -f *.o *.ali ortho_code_main
	$(RM) b~*.ad? *~

distclean: clean


force:

.PHONY: force all clean

ORTHO_BASENAME=ortho_llvm
include $(ortho_srcdir)/Makefile.inc
