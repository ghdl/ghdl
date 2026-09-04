#! /bin/sh

. ../../testenv.sh

# Same defect as issue539, in the shape users hit most often: src/test.vhd
# and tests/test.vhd are both compiled to test.o, so the link used to fail
# with "multiple definition" errors.  GHDL now reports the collision.
# Only the compiled backends (gcc, llvm) write object files; the JIT
# backends (mcode, llvm-jit) do not and are not affected.  See issue1622.
if "$GHDL" --version | grep -q "JIT code generator"; then
  echo "This test needs a backend that writes object files, skipped on a JIT (see issue1622)"
  exit 0
fi

export GHDL_STD_FLAGS="--std=08"

analyze src/test.vhd tests/test.vhd

if OUT=$(elab other_thing 2>&1); then
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
