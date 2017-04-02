#! /bin/sh

. ../../testenv.sh

analyze_failure bug1.vhdl
analyze_failure bug2.vhdl
analyze_failure bug7.vhdl
analyze_failure bug8.vhdl

clean

echo "Test successful"
