#! /bin/sh

. ../../testenv.sh

synth_only repro1
synth_failure repro2.vhdl -e

echo "Test successful"
