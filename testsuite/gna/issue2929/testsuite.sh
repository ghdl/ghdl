#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=93
analyze bit_select.vhdl
elab_simulate bit_select_93vs08_sim

clean

export GHDL_STD_FLAGS=--std=08
analyze bit_select.vhdl
elab_simulate bit_select_93vs08_sim

clean

echo "Test successful"
