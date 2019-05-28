#! /bin/sh

. ../../testenv.sh

synth dff01.vhdl -e dff01
synth dff02.vhdl -e dff02
synth dff03.vhdl -e dff03
synth dff04.vhdl -e dff04
synth dff05.vhdl -e dff05
synth dff06.vhdl -e dff06
synth dff07.vhdl -e dff07
synth dff08.vhdl -e dff08
synth dff09.vhdl -e dff09
synth dff10.vhdl -e dff10

clean

echo "Test successful"
