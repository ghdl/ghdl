#! /bin/sh

. ../../testenv.sh

synth_only repro1
synth_only repro2
synth_only bug
synth_only bug2

echo "Test successful"
