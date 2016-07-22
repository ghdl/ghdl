#! /bin/sh

. ../../testenv.sh

#GHDL_FLAGS=--ieee=synopsys
analyze_failure repro_arith.vhdl 2>&1 | grep "non-standard synopsys"

clean

echo "Test successful"
