#! /bin/sh

. ../../testenv.sh

analyze clock.vhdl bwc.vhdl
analyze_failure sm_tb.vhdl

clean

echo "Test successful"
