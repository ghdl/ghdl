#! /bin/sh

. ../../testenv.sh

synth_failure fileissue.vhdl -e

echo "Test successful"
