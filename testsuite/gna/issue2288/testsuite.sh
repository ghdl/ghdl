#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb_minimal.vhdl
elab_simulate tb_minimal

clean

echo "Test successful"
