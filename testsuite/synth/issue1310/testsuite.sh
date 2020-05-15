#! /bin/sh

. ../../testenv.sh

synth_analyze issue
synth_analyze issue2
clean

echo "Test successful"
