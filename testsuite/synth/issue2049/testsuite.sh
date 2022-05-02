#! /bin/sh

. ../../testenv.sh

synth --std=08 engine.vhdl -e > syn_engine.vhdl

echo "Test successful"
