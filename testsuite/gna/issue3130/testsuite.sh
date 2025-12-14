#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure repro_err.vhdl

if ghdl_is_preelaboration; then
    analyze tb.vhdl
    elab_simulate tb

    clean
fi

echo "Test successful"
