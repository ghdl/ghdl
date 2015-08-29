#! /bin/sh

. ../../testenv.sh

analyze_failure bug.vhdl
analyze_failure bug2.vhdl
analyze_failure bug3.vhdl
clean

echo "Test successful"
