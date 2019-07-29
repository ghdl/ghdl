#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth -fpsl hello.vhdl -e hello > syn_hello.vhdl
analyze syn_hello.vhdl
clean

echo "Test successful"
