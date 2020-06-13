#! /bin/sh

. ../../testenv.sh

analyze_failure mwe_aggr.vhdl
analyze -frelaxed mwe_aggr.vhdl
analyze mwe_case.vhdl
analyze -frelaxed mwe_case.vhdl

clean

echo "Test successful"
