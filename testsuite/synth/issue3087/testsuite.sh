#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_failure example.vhdl example.psl -e

echo "Test successful"
