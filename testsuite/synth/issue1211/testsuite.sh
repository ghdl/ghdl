#! /bin/sh

. ../../testenv.sh

synth_tb repro1
synth_tb delay_ul

echo "Test successful"
