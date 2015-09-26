#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze ent.vhd
elab_simulate ent --stop-time=10ns
clean

echo "Test successful"
