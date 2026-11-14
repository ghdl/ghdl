#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro1.vhdl

analyze A.vhdl B_pkg.vhdl B.vhdl C.vhdl
elab_simulate_failure C

clean

echo "Test successful"
