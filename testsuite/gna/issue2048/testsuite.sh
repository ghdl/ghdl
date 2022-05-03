#! /bin/sh

. ../../testenv.sh

analyze_failure icache.vhdl
analyze_failure icache2.vhdl

clean

echo "Test successful"
