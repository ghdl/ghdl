#! /bin/sh

. ../../testenv.sh

synth_only img01

GHDL_STD_FLAGS=--std=08
synth_only img02
synth_only img03

synth_failure img04.vhdl -e

echo "Test successful"
