#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test.vhdl

analyze repro.vhdl
if ghdl_is_preelaboration; then
    elab_simulate_failure repro
else
    # Should fail, but drivers are not correctly created
    elab_simulate repro
fi
	
clean

echo "Test successful"
