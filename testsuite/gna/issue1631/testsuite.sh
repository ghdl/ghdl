#! /bin/sh

. ../../testenv.sh

analyze_failure bug.vhdl
analyze_failure bug2.vhdl
analyze_failure bug3.vhdl
analyze_failure bug4.vhdl

echo "Test successful"
