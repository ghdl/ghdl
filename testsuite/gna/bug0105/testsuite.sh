#! /bin/sh

. ../../testenv.sh

analyze econcat1.vhdl
elab_simulate econcat1

analyze econcat2.vhdl
elab_simulate econcat2

clean

GHDL_STD_FLAGS="--std=87"

analyze econcat1_87.vhdl
elab_simulate econcat1_87

analyze econcat2_87.vhdl
elab_simulate econcat2_87

clean
echo "Test successful"
