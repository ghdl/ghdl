#! /bin/sh

. ../../testenv.sh

analyze bug.vhdl
analyze_failure bug2.vhdl

clean

echo "Test successful"
