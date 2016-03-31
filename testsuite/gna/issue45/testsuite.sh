#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=08 -fpsl"

analyze test1.vhdl
elab_simulate psl_test_endpoint --psl-report=psl.out

grep -q '"cover-pass": 3' psl.out
rm psl.out

analyze test2.vhdl
elab_simulate psl_test2_endpoint --assert-level=error --expect-failure

analyze_failure endpoint_eval_err.vhdl

analyze endpoint_eval.vhdl
elab_simulate psl_endpoint_eval_in_vhdl

clean

echo "Test successful"
