#! /bin/sh

. ../../testenv.sh

synth_failure issue1.vhdl -e
synth_failure issue2.vhdl -e

synth_tb issue3

clean

echo "Test successful"
