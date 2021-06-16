#! /bin/sh

. ../../testenv.sh


export GHDL_STD_FLAGS=--std=08

synth_tb simple2

synth --out=none imem2a.vhdl -e 2> imem2a.err
grep -q "width: 16 bits, depth: 256" imem2a.err

synth_analyze imem2


clean
echo "Test successful"
