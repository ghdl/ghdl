#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze test1.vhdl
    elab_simulate test

    analyze repro1.vhdl
    elab_simulate repro

    clean
fi

echo "Test successful"
