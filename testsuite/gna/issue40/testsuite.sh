#! /bin/sh

. ../../testenv.sh
analyze_failure test.vhdl
analyze_failure test2.vhdl

clean

echo "Test successful"
