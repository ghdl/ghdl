#! /bin/sh

. ../../testenv.sh

synth_tb func01
synth_only func03

GHDL_STD_FLAGS=--std=08
synth_only func04

synth_tb func06

#TODO: not yet supported by simulation
#synth_tb func07
synth_only func07

echo "Test successful"
