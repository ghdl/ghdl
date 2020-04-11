#! /bin/sh

. ../../testenv.sh

synth -fsynopsys alphablender.vhdl -e > syn_alphablender.vhdl

echo "Test successful"
