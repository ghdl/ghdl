#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
GHDL_SYNTH_FLAGS=--keep-hierarchy=no

synth_only external01
synth_only external02

synth_failure externalerr01.vhdl -e
synth_failure externalerr02.vhdl -e

echo "Test successful"
