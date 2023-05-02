#! /bin/sh

. ../../testenv.sh

synth_only repro1
synth_only repro2
synth_only blackboxexample

echo "Test successful"
