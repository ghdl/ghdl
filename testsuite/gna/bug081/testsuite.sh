#! /bin/sh

. ../../testenv.sh

analyze_failure -Werror=elaboration dummy.vhdl
analyze_failure -Werror=elaboration elab_func.vhdl
analyze_failure --std=08 -Werror=elaboration elab_prot.vhdl
analyze -Werror=elaboration test_comp.vhdl

analyze dummy.vhdl
elab_simulate_failure dummyentity

clean

echo "Test successful"
