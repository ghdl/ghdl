#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze bar_sim.vhdl
elab_simulate bar_sim

clean

echo "Test successful"
