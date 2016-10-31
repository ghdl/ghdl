#! /bin/sh

. ../../testenv.sh

analyze_failure universal_conversion_tb.vhd
analyze_failure repro.vhdl

clean

echo "Test successful"
