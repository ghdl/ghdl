#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only issue_1658
count=$(grep -c gate_anyconst syn_issue_1658.vhdl)
test $count -eq 3

echo "Test successful"
