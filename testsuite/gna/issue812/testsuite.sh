#! /bin/sh

. ../../testenv.sh

# A 2**28 x 128-bit array signal: about 34e9 scalar signals, far more than
# any machine can hold (see the issue thread).  Elaboration must fail; the
# point is how -- a failed allocation reported as such, instead of GHDL's
# internal-error box.
#
# The gcc and llvm backends never get that far: the initial value of the
# signal is built in a stack temporary, so the generated DECL_ELAB has a
# 32 GB stack frame (llvm even warns about it while analysing) and the
# executable dies on stack overflow before allocating anything.  That is a
# different limitation from the one fixed here, so on those backends this
# test only checks that analysis still succeeds.  See issue812.
#
# Elaboration runs under a virtual-memory cap so that the failure is quick
# and cannot disturb the machine.  Where the cap goes depends on the
# backend: the JIT backends (mcode, llvm-jit) allocate the design in
# "ghdl -e" itself, while gcc and llvm only allocate when the elaborated
# executable runs -- and their "ghdl -e" runs the code generator and the
# linker, whose own memory use depends on the toolchain of the machine.
# Capping those made this test pass on some CI runners and fail on others,
# so they are kept out of the cap.
#
# Shells that cannot limit virtual memory (eg macOS) skip the check rather
# than run unbounded.
export GHDL_STD_FLAGS="--std=08 -fsynopsys"

analyze cosim_test.vhd

if ! "$GHDL" --version | grep -q "JIT code generator"; then
  clean
  echo "Test successful"
  exit 0
fi

if ! ( ulimit -v 2000000 ) 2>/dev/null; then
  echo "skipped: this shell cannot limit virtual memory (ulimit -v)"
  clean
  echo "Test successful"
  exit 0
fi

if "$GHDL" --version | grep -q "JIT code generator"; then
  out=$( ( ulimit -v 2000000; elab_simulate cosim_test 2>&1 ) ) || true
else
  elab cosim_test
  out=$( ( ulimit -v 2000000; simulate cosim_test 2>&1 ) ) || true
fi
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: reported an internal error instead of running out of memory"
  exit 1
fi

if ! echo "$out" | grep -qE "out of memory|too large"; then
  case "$out" in
    *error*|*Error*)
      echo "FAIL: expected the size-overflow or out-of-memory diagnostic"
      exit 1
      ;;
    *)
      #  Nothing failed at all: the shell accepted "ulimit -v" but the
      #  limit is not enforced, so the design was never short of memory.
      echo "skipped: the virtual-memory cap is not enforced here"
      ;;
  esac
fi

clean

echo "Test successful"
