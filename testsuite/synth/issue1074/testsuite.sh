#! /bin/sh

. ../../testenv.sh

synth --expect-failure blinky.vhdl -e

echo "Test successful"
