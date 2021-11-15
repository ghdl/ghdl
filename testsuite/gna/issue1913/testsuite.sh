#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze i2c_tb.vhdl
elab_simulate_failure i2c_tb -gg_log_file=log.txt

clean

echo "Test successful"
