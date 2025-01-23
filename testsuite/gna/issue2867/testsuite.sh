#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze force_tb.vhdl
elab_simulate force_tb --stop-time=10us

clean

echo "Test successful"
