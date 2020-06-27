#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro1.vhdl
elab_simulate repro1_tb

analyze pkg.vhdl
analyze window_splitter_1D.vhdl
elab_simulate window_splitter_1D_tb

clean

echo "Test successful"
