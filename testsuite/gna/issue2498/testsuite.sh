#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze i2c_tb.vhdl
elab_simulate i2c_tb

clean

echo "Test successful"
