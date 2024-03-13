#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze bug.vhdl
    analyze testbench.vhdl
    elab_simulate tb_ghdl_bug

    clean
fi

echo "Test successful"
