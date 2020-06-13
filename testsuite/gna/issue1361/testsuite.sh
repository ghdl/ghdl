#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro

analyze repro2.vhdl
elab_simulate repro2

clean

export GHDL_STD_FLAGS=--std=08
analyze repro3.vhdl
elab_simulate repro3

clean

echo "Test successful"
