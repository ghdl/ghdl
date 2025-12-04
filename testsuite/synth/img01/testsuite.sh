#! /bin/sh

. ../../testenv.sh

synth_only img01

GHDL_STD_FLAGS=--std=08
synth_only img02
synth_only img03

synth_failure img04.vhdl -e
synth_failure img05.vhdl -e
synth_failure img06.vhdl -e
synth_failure img07.vhdl -e
synth_failure img08.vhdl -e

echo "Test successful"
