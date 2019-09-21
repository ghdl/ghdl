#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
synth dff.vhdl -e dff > syn_dff.vhdl

echo "Test successful"
