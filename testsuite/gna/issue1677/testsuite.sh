#! /bin/sh

. ../../testenv.sh

analyze_failure -Werror repro.vhdl
analyze repro.vhdl

clean

echo "Test successful"
