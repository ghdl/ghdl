#! /bin/sh

. ../../testenv.sh

analyze test_case.vhd
analyze scrambler_tb.vhd
elab_simulate scrambler_tb

clean

echo "Test successful"
