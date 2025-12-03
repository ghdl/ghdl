#! /bin/sh

. ../../testenv.sh

synth_only tobv01
synth_failure -Werror tobv01.vhdl -e

echo "Test successful"
