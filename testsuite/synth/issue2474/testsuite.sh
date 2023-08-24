#! /bin/sh

. ../../testenv.sh

# Also see issue1428

synth_only repro1
synth_only repro2
synth_only repro3
synth_only repro4

echo "Test successful"
