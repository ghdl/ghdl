#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze type_declaration_pkg.vhd
analyze type_user_pkg.vhd
analyze test_tb.vhd
elab_simulate test_tb

clean

echo "Test successful"
