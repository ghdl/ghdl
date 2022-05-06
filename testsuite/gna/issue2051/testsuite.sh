#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
analyze repro3.vhdl
elab_simulate repro3
clean

GHDL_STD_FLAGS=--std=08
analyze repro2.vhdl
elab_simulate repro2

clean

echo "Test successful"
