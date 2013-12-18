#! /bin/sh

. ../../testenv.sh

analyze TESTCASE.vhdl
elab_simulate_failure testcase

clean

echo "Test successful"
