#! /bin/sh

. ../../testenv.sh

synth_failure rvapo.vhdl top.vhdl -e

echo "Test successful"
