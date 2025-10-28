#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze a.vhdl
elab_simulate a

analyze d.vhdl
elab_simulate d

clean

echo "Test successful"
