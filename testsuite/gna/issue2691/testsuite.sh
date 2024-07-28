#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze --work=colibri synchro.vhdl
    analyze synchro_tb1.vhdl
    elab_simulate synchro_tb --stop-time=1us

    clean
    clean colibri
fi

echo "Test successful"
