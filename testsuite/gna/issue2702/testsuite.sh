#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze repro1.vhdl
    elab_simulate repro1

    analyze top.vhdl
    elab_simulate top

    clean
fi

echo "Test successful"
