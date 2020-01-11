#! /bin/sh

. ../../testenv.sh

synth --expect-failure ent.vhdl -e

echo "Test successful"
