#! /bin/sh

. ../../testenv.sh

analyze_failure tb2.vhdl
analyze_failure tb.vhdl
analyze_failure tb3.vhdl
analyze_failure tb4.vhdl
analyze_failure tb5.vhdl

clean

echo "Test successful"
