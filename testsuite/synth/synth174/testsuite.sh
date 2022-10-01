#! /bin/sh

. ../../testenv.sh

synth_only repro
synth_failure -gw=0 repro.vhdl -e

echo "Test successful"
