#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-Wno-binding
synth_analyze top1
synth_analyze top2

clean

echo "Test successful"
