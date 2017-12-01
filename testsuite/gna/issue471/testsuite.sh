#! /bin/sh

. ../../testenv.sh

$GHDL -i a.vhdl b.vhdl
analyze_failure a.vhdl
analyze_failure b.vhdl

$GHDL -i p1.vhdl
analyze_failure p1.vhdl

clean

echo "Test successful"
