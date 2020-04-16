#! /bin/sh

. ../../testenv.sh

synth -gdepth=1 delayline.vhdl -e > syn_delayline.vhdl

echo "Test successful"
