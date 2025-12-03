#! /bin/sh

. ../../testenv.sh

synth_failure err01.vhdl -e
synth_failure err02.vhdl -e
synth_failure err_slv1.vhdl -e
synth_failure --std=08 err_slv2.vhdl -e
synth_failure err_vect1.vhdl -e

synth_only case01

echo "Test successful"
