#! /bin/sh

. ../../testenv.sh

analyze_failure bug.vhdl
analyze_failure bug3.vhdl
analyze bug2.vhdl

clean

echo "Test successful"
