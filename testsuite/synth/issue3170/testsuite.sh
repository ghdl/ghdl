#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_tb repro_case_qm


echo "Test successful"
