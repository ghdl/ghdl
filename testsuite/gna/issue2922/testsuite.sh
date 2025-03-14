#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro1.vhdl
elab_simulate repro1

analyze bit_select_sim.vhdl
elab_simulate bit_select_sim

clean

echo "Test successful"
