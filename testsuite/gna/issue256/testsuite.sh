#! /bin/sh

. ../../testenv.sh

analyze testcase.vhd
analyze testcase_testbench.vhd
elab_simulate testcase_testbench

analyze testcase2.vhd
analyze testcase2_testbench.vhd
elab_simulate testcase2_testbench

analyze testcase3.vhd

clean

echo "Test successful"
