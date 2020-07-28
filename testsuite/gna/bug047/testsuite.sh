#! /bin/sh

. ../../testenv.sh

#GHDL_STD_FLAGS=--ieee=synopsys
analyze_failure repro_arith.vhdl

clean

echo "Test successful"
