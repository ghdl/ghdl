#! /bin/sh

. ../../testenv.sh

synth_analyze tb
synth_analyze tb2
clean

echo "Test successful"
