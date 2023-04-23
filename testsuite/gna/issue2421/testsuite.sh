#! /bin/sh

. ../../testenv.sh

analyze_failure --std=93 top.vhdl
analyze_failure --std=08 top.vhdl

analyze top.vhdl
elab_simulate top

clean

GHDL_STD_FLAGS="--std=08 -frelaxed"
analyze top.vhdl
elab_simulate top

clean

echo "Test successful"
