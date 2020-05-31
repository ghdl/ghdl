#! /bin/sh

. ../../testenv.sh

synth_analyze tdp_ram
synth_tb ram3
clean

echo "Test successful"
