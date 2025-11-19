#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze DAC_package.vhdl DAC_test.vhdl
elab_simulate dac_test

clean

echo "Test successful"
