#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze foo.vhdl tb_foo.vhdl
elab_simulate tb_foo

analyze foo2.vhdl
elab_simulate foo2

analyze foo3.vhdl
elab_simulate foo3

clean

echo "Test successful"
