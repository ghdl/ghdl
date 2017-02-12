#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
#analyze test.vhdl
#elab_simulate test

analyze cst.vhdl pkg.vhdl
analyze var1.vhdl var2.vhdl var3.vhdl var4.vhdl
analyze assign1.vhdl proc1.vhdl 
elab_simulate proc1

clean

echo "Test successful"
