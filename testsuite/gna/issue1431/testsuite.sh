#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS="-fsynopsys -fexplicit"
analyze config_example.vhdl
elab_simulate config_example

clean

echo "Test successful"
