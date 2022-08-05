#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure tb_systolic.vhdl

clean

echo "Test successful"
