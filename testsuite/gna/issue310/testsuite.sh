#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze b.vhdl
analyze bb.vhdl
analyze bbb.vhdl
elab_simulate bbb

clean

echo "Test successful"
