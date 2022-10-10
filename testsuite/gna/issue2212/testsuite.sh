#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze minimal.vhdl
elab_simulate minimal

clean

echo "Test successful"
