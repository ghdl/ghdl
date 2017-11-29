#! /bin/sh

. ../../testenv.sh

analyze dummy.vhdl
elab_simulate_failure dummyentity

clean

echo "Test successful"
