#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze LinkedListPkg.vhd
analyze Test_LinkedListPkg.vhd
elab_simulate Test_LinkedListPkg

clean

echo "Test successful"
