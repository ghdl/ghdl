#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze bug_tb.vhdl
    elab_simulate ghdl_bug_tb

    clean
fi

echo "Test successful"
