#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

for test in psl_prev psl_stable psl_rose psl_fell psl_onehot psl_onehot0; do
  synth_analyze $test
  analyze tb_${test}.vhdl
  elab_simulate_failure tb_${test} --stop-time=20ns --asserts=disable-at-0 --assert-level=error
  elab_simulate tb_${test} --stop-time=10ns --asserts=disable-at-0 --assert-level=error
done

analyze_failure psl_fell_err1.vhdl
analyze_failure psl_onehot_err.vhdl

clean

echo "Test successful"
