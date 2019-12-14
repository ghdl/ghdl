#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze mixer_pkg.vhdl mixer.vhdl mixer_tb.vhdl
elab_simulate mixer_tb

clean

echo "Test successful"
