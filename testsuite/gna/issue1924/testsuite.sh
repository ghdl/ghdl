#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze fixed_pkg.vhdl float32_pkg.vhdl test_float_to_sfixed.vhdl
elab_simulate test_float_to_sfixed

clean

echo "Test successful"
