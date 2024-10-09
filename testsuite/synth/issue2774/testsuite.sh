#! /bin/sh

. ../../testenv.sh

synth underscore.v -e > syn_underscore.vhdl

analyze syn_underscore.vhdl

clean

echo "Test successful"
