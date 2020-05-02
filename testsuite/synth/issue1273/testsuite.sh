#! /bin/sh

. ../../testenv.sh

synth_analyze issue
synth_tb assert2
synth_tb assert3

echo "Test successful"
