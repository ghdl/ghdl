#! /bin/sh

. ../../testenv.sh

synth --expect-failure --std=08 fixed_point_example.vhdl -e

echo "Test successful"
