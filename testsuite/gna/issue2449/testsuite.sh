#! /bin/sh

. ../../testenv.sh

# KNOWN, NOT-YET-FIXED LIMITATION (see issue.md): a signal whose subtype
# comes from a generic package instantiation -- "subtype lfsr_t is
# bit_vector(TAPS'range)" in a package generic over TAPS -- has run-time
# type information the wave writers cannot walk.  The design runs fine
# without a dumper.
#
# All three backends are affected and they fail differently, so this
# asserts the failure itself rather than any particular message:
#   mcode      raises CONSTRAINT_ERROR in grt-rtis_utils and prints GHDL's
#              bug banner for --wave, and dereferences a null pointer for
#              --vcd (but happens to survive --fst);
#   gcc, llvm  abort with SIGABRT and no output at all for --wave, and
#              dereference a null pointer for --vcd and --fst.
# Asserts the current (broken) behaviour so a future fix is noticed here.
# See issue2449.
export GHDL_STD_FLAGS="--std=08"

analyze counter.vhd

#  Without a dumper the design must run cleanly: the limitation is in the
#  wave writers, not in the design.
elab_simulate counter

for opt in "--wave=counter.ghw" "--vcd=counter.vcd"; do
  if OUT=$(elab_simulate counter $opt 2>&1); then
    echo "$OUT"
    echo "UNEXPECTED PASS: issue2449 appears to be fixed for $opt -- update"
    echo "this test to assert success instead of the known failure."
    exit 1
  fi
  echo "$OUT"
  rm -f counter.ghw counter.vcd
done

clean

#  Same defect with the package instantiated at library level.  It fails in
#  Grt.Waves.Get_Signal_Number instead, which is the signature of #2416.
analyze libinst.vhd

if OUT=$(elab_simulate top3 --wave=top3.ghw --stop-time=10ns 2>&1); then
  echo "$OUT"
  echo "UNEXPECTED PASS: the library-level shape appears to be fixed -- update"
  echo "this test to assert success instead of the known failure."
  exit 1
fi
echo "$OUT"
rm -f top3.ghw

clean

echo "Test successful (known limitation still present, as expected)"
