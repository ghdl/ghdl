#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze th.vhdl tb2.vhdl
    elab_simulate tb2 --stop-time=100ns

    analyze tb.vhdl
    elab_simulate_failure tb --stop-time=100ns
    clean
fi

echo "Test successful"
