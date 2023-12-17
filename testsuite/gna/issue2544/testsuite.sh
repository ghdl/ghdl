#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze generic_fixed_delay.vhdl
    analyze_failure tb-crash.vhdl

    analyze tb.vhdl
    elab_simulate tb

    clean
fi

echo "Test successful"
