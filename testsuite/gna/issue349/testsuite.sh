#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate repro


#analyze ResolutionPkg.vhd
#analyze resolution-tb.vhdl
#elab_simulate resolution_tb

clean

echo "Test successful"
