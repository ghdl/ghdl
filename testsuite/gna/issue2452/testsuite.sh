#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze foo.vhdl tb_foo.vhdl
elab_simulate tb_foo

clean

echo "Test successful"
