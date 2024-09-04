#! /bin/sh

. ../../testenv.sh

synth_failure top.vhdl -e top

echo "Test successful"
