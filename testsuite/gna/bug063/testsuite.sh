#! /bin/sh

. ../../testenv.sh

analyze_failure dff.vhdl 2> dff.out
ghdl_diff_stcr dff.out dff.expected

rm -f dff.out
clean

echo "Test successful"
