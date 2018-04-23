#! /bin/sh

. ../../testenv.sh

analyze_failure assemble.vhdl
analyze assemble2.vhdl

clean

echo "Test successful"
