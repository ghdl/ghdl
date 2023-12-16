#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze repro1.vhdl
    elab_simulate top

    clean

    analyze bug.vhdl
    elab_simulate tb

    clean
fi

echo "Test successful"
