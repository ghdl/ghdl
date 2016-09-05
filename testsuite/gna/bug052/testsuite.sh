#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze tb_simple.vhdl
elab_simulate tb_simple

analyze tb_simple1.vhdl
elab_simulate tb_simple1

analyze tb_simple2.vhdl
elab_simulate tb_simple2

clean

echo "Test successful"
