#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth_tb ram01

#  The verilog writer must keep the order of the ports: write then read,
#  in the same always block.
synth --out=verilog ram01.vhdl -e > syn_ram01.v
fgrep -q "mem[wa] = d;" syn_ram01.v

echo "Test successful"
