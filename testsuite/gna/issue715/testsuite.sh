#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze LRAM.vhdl
elab_simulate LRAM --max-stack-alloc=0

clean

echo "Test successful"
