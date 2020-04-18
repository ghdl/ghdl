#! /bin/sh

. ../../testenv.sh

synth_failure issue1.vhdl -e
synth_failure issue2.vhdl -e

clean

echo "Test successful"
