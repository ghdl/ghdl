#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze modules.vhdl
#elab_simulate module_1

clean

echo "Test successful"
