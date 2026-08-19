#! /bin/sh

. ../../testenv.sh

# Statically evaluating integer'image (IIR_PREDEFINED_INTEGER_TO_STRING)
# during --synth's constant folding used to fail with "synth_static_
# monadic_predefined: unhandled IIR_PREDEFINED_INTEGER_TO_STRING" and
# then crash with an internal TYPES.INTERNAL_ERROR. The original report
# hit this via the third-party JSON-for-VHDL library (not reproduced
# here, as it needs a network fetch); this exercises the same named
# predefined function directly and self-contained. See issue1661.
synth repro.vhdl -e repro > syn_repro.vhdl
analyze syn_repro.vhdl

# "42" has length 2: the folded constant must be std_logic_vector(2), i.e.
# "00000010".
if ! grep -q '"00000010"' syn_repro.vhdl; then
  echo "FAIL: expected the folded length-of-\"42\" constant (2) in the netlist"
  exit 1
fi

clean

echo "Test successful"
