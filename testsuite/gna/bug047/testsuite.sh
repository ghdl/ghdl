#! /bin/sh

. ../../testenv.sh

#GHDL_FLAGS=--ieee=synopsys
analyze_failure repro_arith.vhdl

clean

echo "Test successful"
