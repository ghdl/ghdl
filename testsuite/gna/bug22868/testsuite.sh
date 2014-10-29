#! /bin/sh

. ../../testenv.sh

analyze_failure fails1.vhdl
analyze_failure fails2.vhdl
analyze works.vhdl
clean

echo "Test successful"
