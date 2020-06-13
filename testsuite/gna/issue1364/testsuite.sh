#! /bin/sh

. ../../testenv.sh

analyze_failure --std=93 mwe_aggr.vhdl
analyze mwe_aggr.vhdl
analyze --std=93 mwe_case.vhdl
analyze mwe_case.vhdl

clean

echo "Test successful"
