#! /bin/sh

. ../../testenv.sh

analyze_failure repro1.vhdl
analyze_failure repro2.vhdl

clean

echo "Test successful"
