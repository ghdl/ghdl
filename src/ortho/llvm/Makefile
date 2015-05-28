ortho_srcdir=..
GNAT_FLAGS=-gnaty3befhkmr -gnata -gnatf -gnatwael -gnat05
CXX=clang++
LLVM_CONFIG=llvm-config
SED=sed
BE=llvm

all: $(ortho_exec)

$(ortho_exec): $(ortho_srcdir)/llvm/ortho_llvm.ads force llvm-cbindings.o
	gnatmake -o $@ -aI$(ortho_srcdir)/llvm -aI$(ortho_srcdir) \
	$(GNAT_FLAGS) ortho_code_main -bargs -E \
	-largs llvm-cbindings.o --LINK=$(CXX) \
	`$(LLVM_CONFIG) --ldflags --libs --system-libs` $(LDFLAGS)

llvm-cbindings.o: $(ortho_srcdir)/llvm/llvm-cbindings.cpp
	$(CXX) -c  -I`$(LLVM_CONFIG) --includedir --cflags --cxxflags` -o $@ $<

clean:
	$(RM) -f *.o *.ali ortho_code_main
	$(RM) b~*.ad? *~

distclean: clean


force:

.PHONY: force all clean

ORTHO_BASENAME=ortho_llvm
include $(ortho_srcdir)/Makefile.inc
