#! /bin/sh

. ../../testenv.sh

synth_analyze issue
synth_analyze assert2
synth_analyze assert3
synth_tb assert4
synth_tb assert5
synth_failure assert6
synth_analyze assert7

clean

echo "Test successful"
