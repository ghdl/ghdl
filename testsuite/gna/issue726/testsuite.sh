#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then

    export GHDL_STD_FLAGS=--std=08
    analyze ea-GenericMux.vhdl
    analyze tbMux2.vhdl
    elab_simulate tbMux

    clean
fi


echo "Test successful"
