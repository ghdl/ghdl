#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze eu_tb.vhdl
elab_simulate eu_tb

clean

echo "Test successful"
