#! /bin/sh

. ../../testenv.sh

synth_failure --std=08 -gfifo_depth=3 fifo.vhdl axis_conv1d9x1.vhdl -e

echo "Test successful"
