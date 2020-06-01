#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_analyze issue
analyze tb_issue.vhdl
elab_simulate_failure tb_issue --stop-time=20ns --asserts=disable-at-0 --assert-level=error
elab_simulate tb_issue --stop-time=10ns --asserts=disable-at-0 --assert-level=error

clean

echo "Test successful"
