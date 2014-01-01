#! /bin/sh

. ../../testenv.sh

analyze cnt.vhdl
elab_simulate_failure cnt_v_tb

clean

echo "Test successful"
