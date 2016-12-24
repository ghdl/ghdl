ortho_srcdir=..
GNAT_FLAGS=-gnaty3befhkmr -gnata -gnatf -gnatwael -gnat05
CXX=clang++
LLVM_CONFIG=llvm-config
GNATMAKE=gnatmake
SED=sed
BE=llvm-nodebug

all: $(ortho_exec)

$(ortho_exec): $(ortho_srcdir)/llvm-nodebug/ortho_llvm.ads force llvm-cbindings.o ortho_code_main.adb
	$(GNATMAKE) -o $@ -aI$(ortho_srcdir)/llvm-nodebug -aI$(ortho_srcdir) \
	$(GNAT_FLAGS) ortho_code_main -bargs -E \
	-largs llvm-cbindings.o --LINK=$(CXX) \
	$(LDFLAGS) `$(LLVM_CONFIG) --ldflags --libs --system-libs`

llvm-cbindings.o: $(ortho_srcdir)/llvm-nodebug/llvm-cbindings.cpp
	$(CXX) -c `$(LLVM_CONFIG) --cxxflags` -o $@ $<

ortho_code_main.adb:
	echo "with Ortho_Code_Main$(llvm_be_ver);" > ortho_code_main.tmp
	echo "procedure Ortho_Code_Main is" >> ortho_code_main.tmp
	echo "begin" >> ortho_code_main.tmp
	echo "   Ortho_Code_Main$(llvm_be_ver);" >> ortho_code_main.tmp
	echo "end Ortho_Code_Main;" >> ortho_code_main.tmp
	mv ortho_code_main.tmp $@

clean:
	$(RM) -f *.o *.ali ortho_code_main
	$(RM) b~*.ad? *~

distclean: clean


force:

.PHONY: force all clean

ORTHO_BASENAME=ortho_llvm
include $(ortho_srcdir)/Makefile.inc
