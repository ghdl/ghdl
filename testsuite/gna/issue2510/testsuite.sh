#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze repro1.vhdl
elab_simulate_failure repro1

analyze repro2.vhdl

analyze repro3.vhdl

if ghdl_is_preelaboration; then
    elab_simulate_failure repro3
fi


clean

echo "Test successful"
