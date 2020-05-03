#! /bin/sh

. ../../testenv.sh

synth_analyze int_operators
clean

synth_tb prio02

echo "Test successful"
