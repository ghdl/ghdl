#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze ent.vhdl
    elab_simulate ent

    clean
fi

echo "Test successful"
