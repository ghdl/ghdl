#! /bin/sh

. ../../testenv.sh

analyze_failure -Werror=elaboration dummy.vhdl

analyze dummy.vhdl
elab_simulate_failure dummyentity

clean

echo "Test successful"
