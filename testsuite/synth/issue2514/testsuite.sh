#! /bin/sh

. ../../testenv.sh

synth --std=08 -Werror test.vhdl -e > syn_test.vhdl

echo "Test successful"
