ortho_srcdir=..
GNATFLAGS=-gnaty3befhkmr -gnata -gnatf -gnatwael
CXX=clang++
LLVM_CONFIG=llvm-config
GNATMAKE=gnatmake
SED=sed
BE=llvm4-nodebug

LLVM_LDFLAGS=$(LLVM_CONFIG) --ldflags --libs --system-libs

all: $(ortho_exec)

$(ortho_exec): $(ortho_srcdir)/llvm4-nodebug/ortho_llvm.ads force llvm-cbindings.o
	$(GNATMAKE) -o $@ -aI$(ortho_srcdir)/llvm4-nodebug -aI$(ortho_srcdir) \
	$(GNATFLAGS) ortho_code_main -bargs -E \
	-largs llvm-cbindings.o --LINK=$(CXX) \
	`$(LLVM_LDFLAGS)` $(LDFLAGS)

llvm-cbindings.o: $(ortho_srcdir)/llvm4-nodebug/llvm-cbindings.cpp
	$(CXX) -c `$(LLVM_CONFIG) --cxxflags` -o $@ $<

clean:
	$(RM) -f *.o *.ali ortho_code_main
	$(RM) b~*.ad? *~

distclean: clean


force:

.PHONY: force all clean

ORTHO_BASENAME=ortho_llvm
include $(ortho_srcdir)/Makefile.inc
