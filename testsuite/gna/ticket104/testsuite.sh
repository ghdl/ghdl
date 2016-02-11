#! /bin/sh

. ../../testenv.sh

analyze bug_tb.vhd
# Use stop-time as a guard.
elab_simulate_failure bug_tb --stop-time=100ns

clean

echo "Test successful"
