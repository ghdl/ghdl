#! /bin/sh

. ../../testenv.sh

# A std_logic_vector sized from a generic near INTEGER'HIGH.  It needs
# 2**31 signals, ie 16 GB of signal storage, so elaboration must fail -- the
# point is that it fails with a clean diagnostic instead of GHDL's
# internal-error box.  Which diagnostic depends on what gives way first: the
# backends that describe the object report that its size does not fit on 32
# bits, the others run out of memory.  Both are clean errors.
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
export GHDL_STD_FLAGS="--std=08"

analyze example.vhd

W=-gVEC_WIDTH=2147483647

if ! ( ulimit -v 2000000 ) 2>/dev/null; then
  echo "skipped: this shell cannot limit virtual memory (ulimit -v)"
  clean
  echo "Test successful"
  exit 0
fi

if "$GHDL" --version | grep -q "JIT code generator"; then
  out=$( ( ulimit -v 2000000; elab_simulate example $W 2>&1 ) ) || true
else
  elab example
  out=$( ( ulimit -v 2000000; simulate example $W 2>&1 ) ) || true
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
