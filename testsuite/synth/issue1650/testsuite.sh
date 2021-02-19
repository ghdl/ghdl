#! /bin/sh

. ../../testenv.sh

synth_failure --vendor-library=missing_lib debounce.vhdl -e

synth_only debounce

echo "Test successful"
