#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze protocol_pkg.vhd node.vhd top.vhd tb.vhd
elab_simulate tb

clean

echo "Test successful"
