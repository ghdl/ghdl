#! /bin/sh

. ../../testenv.sh

synth_tb tri
synth_tb multiplexers_3

echo "Test successful"
