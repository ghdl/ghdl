#! /bin/sh

. ../../testenv.sh

analyze_failure ent.vhdl
analyze_failure repro.vhdl
analyze_failure --force-analysis repro.vhdl
analyze_failure repro2.vhdl
analyze_failure repro3.vhdl

clean

echo "Test successful"
