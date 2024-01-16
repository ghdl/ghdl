#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze test2.vhdl
elab_simulate test2

if ghdl_is_preelaboration; then
    analyze ent.vhdl
    elab_simulate test
fi

clean


echo "Test successful"
