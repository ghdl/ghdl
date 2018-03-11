#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze pixel_pkg.vhd pixel_column_pkg.vhd pixel_matrix_pkg.vhd test_op.vhd
elab_simulate test_op

clean

echo "Test successful"
