#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
#analyze test.vhdl
#elab_simulate test

analyze cst.vhdl pkg.vhdl
analyze var1.vhdl var2.vhdl var3.vhdl var4.vhdl
analyze assign1.vhdl proc1.vhdl
elab_simulate proc1

analyze sig1.vhdl
elab_simulate sig1

analyze call1.vhdl
elab_simulate call1

analyze call2.vhdl
elab_simulate call2

analyze call3.vhdl
elab_simulate call3

analyze call4.vhdl
elab_simulate call4

analyze call5.vhdl
elab_simulate call5

clean

echo "Test successful"
