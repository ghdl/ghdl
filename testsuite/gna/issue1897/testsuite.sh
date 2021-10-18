#! /bin/sh

. ../../testenv.sh

run $GHDL -i --std=08 wb_master_BFM.vhd wb_master_BFM_v1_0.vhd test_2_tb.vhd
run $GHDL -m --expect-failure --std=08 test_2_tb

clean --std=08

echo "Test successful"
