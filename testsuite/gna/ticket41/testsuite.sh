#! /bin/sh

. ../../testenv.sh

analyze bug.vhdl
analyze_failure bug1.vhdl

clean

echo "Test successful"
