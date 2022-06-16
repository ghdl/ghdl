#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze my_fixed_pkg.vhdl

analyze tb_fixed1.vhdl
elab_simulate tb_fixed1

analyze tb_fixed2.vhdl
elab_simulate tb_fixed2

analyze_failure tb_fixed3.vhdl

clean

echo "Test successful"
