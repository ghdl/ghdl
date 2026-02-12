#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=19
    analyze ent.vhdl
    elab_simulate tb_axis_sim --stop-time=1us

    clean
fi

echo "Test successful"
