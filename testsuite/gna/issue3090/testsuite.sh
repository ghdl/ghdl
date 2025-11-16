#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb.vhdl
elab_simulate tb_ghdl

analyze tb_proc.vhdl
elab_simulate tb_proc

clean

echo "Test successful"
