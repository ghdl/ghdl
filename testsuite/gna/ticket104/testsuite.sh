#! /bin/sh

. ../../testenv.sh

analyze bug_tb.vhd
elab_simulate_failure bug_tb

clean

echo "Test successful"
