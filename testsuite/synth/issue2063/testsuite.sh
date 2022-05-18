#! /bin/sh

. ../../testenv.sh

synth_failure array_index_crash.vhdl -e

echo "Test successful"
