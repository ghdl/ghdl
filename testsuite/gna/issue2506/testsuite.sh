#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze repro1.vhdl

    analyze A.vhdl B_pkg.vhdl B.vhdl C.vhdl
    elab_simulate_failure C

    clean
fi

echo "Test successful"
