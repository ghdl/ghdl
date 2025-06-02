#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze demo1.vhdl
elab_simulate demo1

analyze demo_tb.vhdl
elab_simulate demo_tb

clean

echo "Test successful"
