#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze min_gen_pkg_tb_e.vhdl
elab_simulate min_gen_pkg_tb_e

clean

analyze min_gen_pkg_tb_e2.vhdl
elab_simulate min_gen_pkg_tb_e

clean

echo "Test successful"
