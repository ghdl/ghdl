#! /bin/sh

. ../../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze pixel_pkg.vhd pixel_column_pkg.vhd test_op1.vhd
elab_simulate test_op1

clean

echo "Test successful"
