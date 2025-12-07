#! /bin/sh

. ../../testenv.sh

synth_tb func01
synth_only func03

GHDL_STD_FLAGS=--std=08
synth_only func04

echo "Test successful"
