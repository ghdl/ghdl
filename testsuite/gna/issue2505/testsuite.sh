#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze top_tb.vhdl
    elab_simulate top_tb

    clean
fi

echo "Test successful"
