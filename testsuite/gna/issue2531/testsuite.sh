#! /bin/sh

. ../../testenv.sh

analyze_failure repro.vhdl
analyze repro2.vhdl

clean

echo "Test successful"
