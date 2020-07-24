#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth aes_pkg.vhdl aes_enc.vhdl -e > syn_aes.vhdl

echo "Test successful"
