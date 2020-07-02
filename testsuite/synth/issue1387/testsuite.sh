#! /bin/sh

. ../../testenv.sh

synth_failure repro1.vhdl -e

synth_analyze repro2

echo "Test successful"
