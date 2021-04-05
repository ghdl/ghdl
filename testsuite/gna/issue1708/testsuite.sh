#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure tb_top.vhdl
analyze_failure ex3.vhdl
analyze_failure ex4.vhdl

analyze ex1.vhdl
elab_simulate ex1

analyze ex2.vhdl
elab_simulate ex2

analyze ex5.vhdl
elab_simulate ex5

clean

echo "Test successful"
