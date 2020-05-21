#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="-fsynopsys -fexplicit"
synth_only fpadd_normalize_struct

echo "Test successful"
