#! /bin/sh

. ../../testenv.sh

synth_failure err01.vhdl -e
synth_failure err02.vhdl -e

synth_only case01

echo "Test successful"
