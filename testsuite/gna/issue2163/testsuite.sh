#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure pkg2.vhdl

analyze pkg1.vhdl
elab_simulate tb_pkg1

analyze pkg3.vhdl
elab_simulate tb_pkg3

clean

echo "Test successful"
