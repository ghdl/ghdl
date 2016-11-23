#! /bin/sh

. ../../testenv.sh

analyze util.vhdl
analyze_failure main.vhdl

analyze main2.vhdl

clean

echo "Test successful"
