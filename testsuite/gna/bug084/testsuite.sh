#! /bin/sh

. ../../testenv.sh

analyze func_test3.vhdl
elab_simulate func_test3

analyze func_test1.vhdl
elab_simulate func_test1

analyze func_test2.vhdl
elab_simulate func_test2

analyze mod5.vhdl
elab_simulate mod5_tb

analyze mod5x.vhdl
elab_simulate mod5x_tb

clean

echo "Test successful"
