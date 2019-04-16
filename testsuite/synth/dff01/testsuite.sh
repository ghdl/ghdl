#! /bin/sh

. ../../testenv.sh

synth dff01.vhdl -e dff01
synth dff02.vhdl -e dff02
synth dff03.vhdl -e dff03
synth dff04.vhdl -e dff04

clean

echo "Test successful"
