#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate repro

analyze err.vhdl
elab_simulate phz_calc

analyze ok.vhdl
elab_simulate foo

clean

echo "Test successful"
