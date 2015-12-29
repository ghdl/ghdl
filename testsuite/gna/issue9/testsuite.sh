#! /bin/sh

. ../../testenv.sh


analyze_failure repro.vhdl
analyze_failure repro2.vhdl

clean test

echo "Test successful"
