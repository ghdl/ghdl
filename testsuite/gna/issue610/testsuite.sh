#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro1.vhdl
elab_simulate repro1

clean

export GHDL_STD_FLAGS=-frelaxed-rules
analyze repro2.vhdl
elab_simulate repro2

analyze repro3.vhdl
#elab_simulate repro3

analyze repro4.vhdl
#elab_simulate repro4

clean

echo "Test successful"
