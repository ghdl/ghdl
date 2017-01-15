#! /bin/sh

. ../../testenv.sh

analyze testcase.vhd
analyze testcase2.vhd
analyze testcase3.vhd
analyze testcase_testbench.vhd
elab_simulate testcase_testbench

clean

echo "Test successful"
