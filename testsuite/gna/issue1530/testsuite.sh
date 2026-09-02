#! /bin/sh

. ../../testenv.sh

# The PSL builtin functions -- stable(), and likewise prev/rose/fell/
# onehot/onehot0 -- are implemented for --synth but not in the translator
# used for ordinary simulation, so a design using one of them ended in
# "translate_expression: cannot handle IIR_KIND_PSL_STABLE" and an
# internal error instead of a diagnostic.  This is a reduced equivalent
# of the original, much larger report.
#
# The diagnostic now appears, but not at the same point on every backend:
# gcc and llvm translate while analysing, so it comes out of "ghdl -a" and
# compilation stops there; the JIT backends (mcode and llvm-jit) translate
# while the simulation is already being elaborated, so the message appears
# then and the generated code traps rather than carry on evaluating the
# assertion on a made-up value.  Either way the point is the same -- a
# clean diagnostic and no internal error.  See issue1530.
export GHDL_STD_FLAGS="--std=08 -fpsl"

if "$GHDL" --version | grep -q "JIT code generator"; then
  analyze t.vhd
  if out=$(elab_simulate t 2>&1); then
    echo "$out"
    echo "UNEXPECTED PASS: PSL builtins now translate for simulation --"
    echo "update this test to assert the simulation result instead."
    exit 1
  fi
  echo "$out"
  if echo "$out" | grep -q "PSL builtin function not supported"; then
    :
  elif ! echo "$out" | grep -q "unsupported construct"; then
    echo "FAIL: expected the run-time trap on the unsupported construct"
    exit 1
  fi
else
  out=$(analyze_failure t.vhd 2>&1)
  echo "$out"

  if ! echo "$out" | grep -q "PSL builtin function not supported"; then
    echo "FAIL: expected the clean not-supported diagnostic"
    exit 1
  fi
fi

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: crashed instead of reporting a clean error"
  exit 1
fi

clean

echo "Test successful"
