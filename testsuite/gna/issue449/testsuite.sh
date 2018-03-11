#! /bin/sh

. ../../testenv.sh

analyze repro1.vhdl
analyze_failure repro2.vhdl

clean

echo "Test successful"
