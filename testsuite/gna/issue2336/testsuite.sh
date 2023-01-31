#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test_issue_ok.vhdl
elab_simulate test_issue_ok --assert-level=error

clean

analyze test_issue_err1.vhdl
elab_simulate_failure test_issue_err1 --assert-level=error

clean

echo "Test successful"
