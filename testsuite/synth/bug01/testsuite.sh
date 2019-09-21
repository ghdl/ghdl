#! /bin/sh

. ../../testenv.sh

synth display.vhdl -e display > syn_display.vhdl
analyze syn_display.vhdl
clean

echo "Test successful"
