#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro_tb.vhdl
elab_simulate repro_tb

analyze repro2_tb.vhdl
elab_simulate repro2_tb

clean

echo "Test successful"
