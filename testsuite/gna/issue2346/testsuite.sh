#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze aggr.vhdl
elab_simulate AggregateWithDelay

analyze aggr4.vhdl
elab_simulate aggr4

clean

analyze aggr2.vhdl
elab_simulate aggr2

analyze aggr3.vhdl
elab_simulate aggr3

clean

echo "Test successful"
