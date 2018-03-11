#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fpsl
analyze tb.vhdl
elab_simulate foo_tb >sim_log.txt

run "grep -q 'falling_a0 custom report' sim_log.txt"
run_failure "grep -q 'sequence covered' sim_log.txt"

rm -f sim_log.txt
clean

echo "Test successful"
