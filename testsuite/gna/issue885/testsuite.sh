#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze a.vhdl
elab_simulate dut

clean

echo "Test successful"
