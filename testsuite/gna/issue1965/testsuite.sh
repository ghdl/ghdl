#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze bar_sim.vhdl
    elab_simulate bar_sim

    clean
fi

echo "Test successful"
