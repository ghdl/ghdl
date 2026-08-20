#! /bin/sh

. ../../testenv.sh

analyze dyn_zero.vhdl
elab_simulate_failure dyn_zero

clean

analyze dyn_zero_signal.vhdl
elab_simulate_failure dyn_zero_signal

clean

echo "Test successful"
