#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze ext_name.vhdl
    elab_simulate ext_name

    clean
fi

echo "Test successful"
