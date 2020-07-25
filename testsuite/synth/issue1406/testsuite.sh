#! /bin/sh

. ../../testenv.sh

synth_analyze half_adder
synth_analyze repro

clean

echo "Test successful"
