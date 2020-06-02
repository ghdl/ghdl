#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze wb_pkg.vhd wb_rst_pkg.vhd memory_map_pkg.vhd
analyze_failure memory_map.vhd

clean

echo "Test successful"
