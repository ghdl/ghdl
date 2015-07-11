#! /bin/sh

. ../../testenv.sh

analyze_failure repro.vhdl

analyze pkg.vhdl
analyze repro1.vhdl
analyze pkg.vhdl
analyze_failure repro2.vhdl

clean

echo "Test successful"
