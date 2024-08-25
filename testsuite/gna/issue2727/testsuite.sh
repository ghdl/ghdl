#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze RandomPkg.vhdl tb_bug.vhdl
elab_simulate tb_bug

clean

echo "Test successful"
