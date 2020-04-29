#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=08 -fpsl"

synth issue.vhdl -e > syn_issue.vhdl

echo "Test successful"
