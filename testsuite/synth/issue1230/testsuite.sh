#! /bin/sh

. ../../testenv.sh

synth -gdepth=1 DelayLine.vhdl -e > syn_delayline.vhdl

echo "Test successful"
