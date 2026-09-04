#! /bin/sh

. ../../testenv.sh

# A nested array signal of 2**16 x 2**16 bytes: 2**35 bytes, far more than
# the 4 GB a size can express.  Sizes are computed on 32 bits, so the
# computation wrapped and __ghdl_malloc0 was asked for 0 bytes; elaboration
# got a valid empty block and wrote past it.  Valgrind called it "Invalid
# write of size 1 [...] 0 bytes after a block of size 0", and what happened
# next was up to the C library -- an unrelated allocation failing later, or
# "malloc(): unaligned tcache chunk detected".  Either the overflow is now
# reported, or the design runs out of memory first; both are clean errors,
# and neither is an internal error box.
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

analyze repro.vhd

if ! ( ulimit -v 2000000 ) 2>/dev/null; then
  echo "skipped: this shell cannot limit virtual memory (ulimit -v)"
  clean
  echo "Test successful"
  exit 0
fi

if "$GHDL" --version | grep -q "JIT code generator"; then
  out=$( ( ulimit -v 2000000; elab_simulate repro 2>&1 ) ) || true
else
  elab repro
  out=$( ( ulimit -v 2000000; simulate repro 2>&1 ) ) || true
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

#  The same overflow with nothing to allocate: only the layout of an
#  oversized subtype is computed, so this needs no memory, no virtual-memory
#  cap and nothing to skip.  Every backend must report the size, and the
#  error must be reported rather than raised: the JIT backends compute the
#  layout while elaborating (simul-vhdl_compile.adb) and the others in the
#  generated code (__ghdl_index_mul).
analyze ovf.vhd

if "$GHDL" --version | grep -q "JIT code generator"; then
  out=$( elab_simulate ovf 2>&1 ) || true
else
  elab ovf
  out=$( simulate ovf 2>&1 ) || true
fi
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: reported an internal error instead of the oversized design"
  exit 1
fi

if ! echo "$out" | grep -q "too large"; then
  echo "FAIL: expected the size-overflow diagnostic"
  exit 1
fi

clean

echo "Test successful"
