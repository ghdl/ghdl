#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

for test in psl_prev psl_stable; do
  synth_analyze $test
  analyze tb_${test}.vhdl
  elab_simulate_failure tb_${test} --stop-time=20ns --asserts=disable-at-0 --assert-level=error
  elab_simulate tb_${test} --stop-time=10ns --asserts=disable-at-0 --assert-level=error
done

clean

echo "Test successful"
