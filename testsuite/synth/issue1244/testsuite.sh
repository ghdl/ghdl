#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=02 -fsynopsys"
synth_failure ram_protected_sharedvar.vhd -e

echo "Test successful"
