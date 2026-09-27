#! /bin/sh

. ../../testenv.sh

# Reading a signal inside an instance through an external name.
#
# Two separate things are checked here.
#
# 1) Simulation.  The pre-elaborated backends (mcode, llvm-jit) support it.
#    gcc and llvm elaborate from generated code, and that path never fills
#    the variable that holds the object -- only Simul.Vhdl_Compile does.
#    It used to leave it null, so the design died either on
#    "NULL access dereferenced" or, when the name reached a sensitivity
#    list, on a bare abort with no message at all.  It must report the
#    unsupported construct instead.
#
# 2) Synthesis (issue #3121).  It works once the instance has been
#    flattened into the design being synthesized, that is with
#    --keep-hierarchy=no *and* with the instantiation synthesized before
#    the statement that reads through the name.  Every other combination is
#    rejected, and the message used to be "cannot use signal value during
#    elaboration", which sends the user looking for a signal read in a
#    declarative part -- not what happened.

export GHDL_STD_FLAGS=--std=08

analyze repro.vhd
analyze ok.vhd

if ghdl_is_preelaboration; then
  elab_simulate repro
  elab_simulate ok
else
  for unit in repro ok; do
    out=$(elab_simulate_failure $unit 2>&1)
    echo "$out"
    if ! echo "$out" | grep -q "unsupported construct"; then
      echo "FAIL: expected the unsupported-construct diagnostic for $unit"
      exit 1
    fi
  done
fi

# The one combination synthesis supports.
synth --keep-hierarchy=no ok > syn_ok.vhdl
if ! grep -q "entity ok" syn_ok.vhdl; then
  echo "FAIL: expected a netlist for ok"
  exit 1
fi

check_diagnostic ()
{
  echo "$1"
  if echo "$1" | grep -q "GHDL Bug occurred"; then
    echo "FAIL: crashed instead of reporting a clean error"
    exit 1
  fi
  if ! echo "$1" | grep -q "external name denotes a signal"; then
    echo "FAIL: expected the external-name diagnostic"
    exit 1
  fi
}

check_diagnostic "$(synth repro 2>&1)"
check_diagnostic "$(synth --keep-hierarchy=no repro 2>&1)"
check_diagnostic "$(synth ok 2>&1)"

rm -f syn_ok.vhdl
clean

echo "Test successful"
