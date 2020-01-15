#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth top.vhdl -e conf > syn_conf.vhdl

echo "Test successful"
