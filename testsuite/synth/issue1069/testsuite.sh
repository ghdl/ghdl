#! /bin/sh

. ../../testenv.sh

synth_tb tdp_ram
synth_tb tdp_ram2
synth_tb ram3
synth_tb ram4
#synth_tb ram41
synth_tb ram5
clean

echo "Test successful"
