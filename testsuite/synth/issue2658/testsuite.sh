#! /bin/sh

. ../../testenv.sh

synth_failure bugtest.vhdl -e

echo "Test successful"
