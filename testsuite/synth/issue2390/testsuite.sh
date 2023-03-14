#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth types.vhdl top.vhdl ram.vhdl ucpu.vhdl rom.vhdl -e > syn_top.vhdl

echo "Test successful"
