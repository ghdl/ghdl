#! /bin/sh

. ../../testenv.sh

analyze_failure e.vhdl
analyze_failure e1.vhdl

clean

echo "Test successful"
