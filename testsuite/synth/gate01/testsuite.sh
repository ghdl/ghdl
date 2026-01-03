#! /bin/sh

. ../../testenv.sh

synth myor.v -e > syn_myor.vhdl

echo "Test successful"
