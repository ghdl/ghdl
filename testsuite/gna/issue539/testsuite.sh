#! /bin/sh

. ../../testenv.sh

# The object file of a design file used to be named after the *base* name
# of its source file, in the library directory, so pkg1/pkg.vhd and
# pkg2/pkg.vhd were both compiled to pkg.o: the second analysis
# overwrote the first object file and the link failed with a pile of
# "multiple definition" messages about symbols the user never wrote.
# The object file now keeps the name and the extension of the source
# file, followed by a number identifying its directory.
# Only the compiled backends (gcc, llvm) write object files; the JIT
# backends (mcode, llvm-jit) do not and are not affected.  See issue539
# (and the identical issue1622).
if "$GHDL" --version | grep -q "JIT code generator"; then
  echo "This test needs a backend that writes object files, skipped on a JIT (see issue539)"
  exit 0
fi

"$GHDL" -i pkg1/pkg.vhd pkg2/pkg.vhd top.vhd
"$GHDL" -m top
run ./top

# One object file per source, not one for both.
if [ "$(ls pkg.vhd-*.o 2> /dev/null | wc -l)" -ne 2 ]; then
  echo "FAIL: expected an object file for each pkg.vhd"
  ls
  exit 1
fi

"$GHDL" --clean
if ls pkg.vhd-*.o > /dev/null 2>&1; then
  echo "FAIL: --clean left the object files behind"
  exit 1
fi

clean

# Two sources of the same directory that differ only by their extension
# are in the same case: example.vhd and example.vht, reported by
# Mercotui on issue539.  The extension is part of the object file name.
"$GHDL" -i ext/pkg.vhd ext/pkg.vhdl ext/top_ext.vhd
"$GHDL" -m top_ext
run ./top_ext

if ! ls pkg.vhd-*.o > /dev/null 2>&1 || ! ls pkg.vhdl-*.o > /dev/null 2>&1; then
  echo "FAIL: the extension must be part of the object file name"
  ls
  exit 1
fi

clean

echo "Test successful"
