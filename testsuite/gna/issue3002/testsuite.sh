#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze top_concat_16k.vhdl

clean

echo "Test successful"
