#! /bin/sh

. ../../testenv.sh

analyze_failure repro.vhdl
analyze_failure repro2.vhdl
analyze_failure repro3.vhdl
analyze_failure repro4.vhdl

clean

echo "Test successful"
