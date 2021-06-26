#! /bin/sh

. ../../testenv.sh

synth --expect-failure ent1.vhdl -e

synth_analyze ent2
clean

echo "Test successful"
