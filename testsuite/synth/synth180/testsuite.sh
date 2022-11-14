#! /bin/sh

. ../../testenv.sh

synth_failure bitbang.vhdl -e
synth_only bitbang2

echo "Test successful"
