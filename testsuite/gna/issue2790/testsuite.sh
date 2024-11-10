#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze mini_repro1.vhdl
elab_simulate mini_repro1

#analyze mini_repro.vhdl
#elab_simulate mini_repro

clean

echo "Test successful"
