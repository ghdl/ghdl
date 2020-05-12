#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze wishbone_types.vhdl icache.vhdl
elab_simulate icache

clean

analyze wishbone_types_err2.vhdl icache.vhdl
elab_simulate icache

clean

echo "Test successful"
