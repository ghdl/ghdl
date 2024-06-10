#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze foo.vhdl
analyze ex_sync.vhdl
analyze ex_async.vhdl

clean

echo "Test successful"
