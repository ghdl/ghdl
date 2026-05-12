#! /bin/sh

. ../../testenv.sh

synth_tb top
synth_tb top2

echo "Test successful"
