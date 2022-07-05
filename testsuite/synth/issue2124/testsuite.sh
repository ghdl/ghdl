#! /bin/sh

. ../../testenv.sh

synth a.vhdl b.vhdl -e a > syn_a.vhdl

synth --expect-failure --std=93 a2.vhdl b.vhdl -e a2

echo "Test successful"
