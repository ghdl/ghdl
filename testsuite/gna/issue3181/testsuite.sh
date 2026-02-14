#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS="--std=08 --finteger64"
    analyze t64.vhdl

    clean

    analyze sim.vhdl
    elab_simulate test_64bit

    clean
fi

echo "Test successful"
