#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
for t in match01 match02 match03 match05 match07 match08; do
    synth_tb $t
done

synth_failure match04.vhdl -e
synth_failure match06.vhdl -e

echo "Test successful"
