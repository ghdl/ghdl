#! /bin/sh

. ../../testenv.sh

analyze_failure tab1.vhdl 2> tab1.err
ghdl_diff_stcr tab1.ref tab1.err

echo "Test successful"
