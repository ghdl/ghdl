#! /bin/sh

. ../../testenv.sh

synth popcount.vhdl -e popcount > syn_popcount.vhdl

echo "Test successful"
