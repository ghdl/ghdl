#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth ent.vhdl -e

echo "Test successful"
