#! /bin/sh

. ../../testenv.sh

analyze_failure repro.vhdl
analyze_failure repro1.vhdl

clean

echo "Test successful"
