#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb_dct8.vhdl
elab_simulate tb_dct8

analyze tb_dct.vhdl
elab_simulate tb_dct8

clean

echo "Test successful"
