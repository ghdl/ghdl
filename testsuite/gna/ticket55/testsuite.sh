#! /bin/sh

. ../../testenv.sh

analyze bug.vhdl
analyze_failure bug1.vhdl
analyze_failure bug2.vhdl
analyze_failure bug3.vhdl

clean

echo "Test successful"
