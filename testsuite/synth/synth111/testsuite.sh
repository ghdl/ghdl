#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth rams_sp_3d.vhdl -e > syn_rams_sp_3d.vhdl
clean

echo "Test successful"
