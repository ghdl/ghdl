#! /bin/sh

. ../../testenv.sh

synth_only repro2

synth_failure repro1.vhdl -e
synth_failure ent.vhdl -e

echo "Test successful"
