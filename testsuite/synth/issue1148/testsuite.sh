#! /bin/sh

. ../../testenv.sh

synth --std=08 --expect-failure test.vhdl -e

echo "Test successful"
