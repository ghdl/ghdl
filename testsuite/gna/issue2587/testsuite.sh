#! /bin/sh

. ../../testenv.sh

analyze_failure tb1.vhdl
analyze_failure tb2.vhdl
analyze_failure tb3.vhdl

clean

echo "Test successful"
