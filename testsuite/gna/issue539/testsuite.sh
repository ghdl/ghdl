#! /bin/sh

. ../../testenv.sh

# The object file of a design file is named after the base name of its
# source file, so pkg1/pkg.vhd and pkg2/pkg.vhd are both compiled to
# pkg.o: the second analysis overwrites the first object file and the
# link used to fail with a pile of "multiple definition" messages about
# symbols the user never wrote.  GHDL now says what is wrong instead.
# Only the compiled backends (gcc, llvm) write object files; the JIT
# backends (mcode, llvm-jit) do not and are not affected.  See issue539
# (and the identical issue1622).
if "$GHDL" --version | grep -q "JIT code generator"; then
  echo "This test needs a backend that writes object files, skipped on a JIT (see issue539)"
  exit 0
fi

"$GHDL" -i pkg1/pkg.vhd pkg2/pkg.vhd top.vhd

if OUT=$("$GHDL" -m top 2>&1); then
  echo "$OUT"
  echo "FAIL: the collision was not reported"
  exit 1
fi
echo "$OUT"

if echo "$OUT" | grep -q "multiple definition"; then
  echo "FAIL: reached the link and got the raw linker errors"
  exit 1
fi

if ! echo "$OUT" | grep -q "are both compiled to"; then
  echo "FAIL: expected the object file collision diagnostic"
  exit 1
fi

clean

echo "Test successful"
