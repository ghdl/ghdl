#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze top.vhdl tb.vhdl
    elab_simulate_failure tb

    clean
fi

echo "Test successful"
