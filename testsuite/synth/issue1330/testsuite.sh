#! /bin/sh

. ../../testenv.sh

synth_tb test
synth_tb test2
synth_tb test3

echo "Test successful"
