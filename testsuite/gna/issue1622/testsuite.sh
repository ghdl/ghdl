#! /bin/sh

. ../../testenv.sh

# Same defect as issue539, in the shape users hit most often: src/test.vhd
# and tests/test.vhd were both compiled to test.o, so the link failed with
# "multiple definition" errors.  The object file now keeps the name and
# the extension of the source file, followed by a number identifying its
# directory.
# Only the compiled backends (gcc, llvm) write object files; the JIT
# backends (mcode, llvm-jit) do not and are not affected.  See issue1622.
if "$GHDL" --version | grep -q "JIT code generator"; then
  echo "This test needs a backend that writes object files, skipped on a JIT (see issue1622)"
  exit 0
fi

export GHDL_STD_FLAGS="--std=08"

analyze src/test.vhd tests/test.vhd
elab_simulate other_thing

if [ "$(ls test.vhd-*.o 2> /dev/null | wc -l)" -ne 2 ]; then
  echo "FAIL: expected an object file for each test.vhd"
  ls
  exit 1
fi

clean

# The number is computed on the path as it is written, so a directory
# that is not below the directory of analysis is not a special case: the
# vhdl libraries of ghdl itself are analyzed as '../../src/ieee/...vhdl'.
# Check a '..' path and an absolute path each get their own object file,
# and that a source without any directory keeps a plain name (that one is
# how std_standard is compiled).
mkdir -p build
"$GHDL" -a --std=08 --workdir=build ../issue1622/src/test.vhd
"$GHDL" -a --std=08 --workdir=build "$(pwd)/tests/test.vhd"
if [ "$(ls build/test.vhd-*.o 2> /dev/null | wc -l)" -ne 2 ]; then
  echo "FAIL: expected an object file for each path"
  ls build
  exit 1
fi
rm -rf build

mkdir -p build
(cd src; "$GHDL" -a --std=08 --workdir=../build test.vhd)
if [ ! -f build/test.vhd.o ]; then
  echo "FAIL: a source without a directory must keep a plain object name"
  ls build
  exit 1
fi
rm -rf build

echo "Test successful"
