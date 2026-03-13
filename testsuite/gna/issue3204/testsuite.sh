#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze t4.vhdl
clean

analyze t3.vhdl tb.vhdl
elab_simulate tb

clean

echo "Test successful"
