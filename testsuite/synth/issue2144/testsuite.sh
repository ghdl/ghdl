#! /bin/sh

. ../../testenv.sh

synth repro.vhdl -e > syn_repro.vhdl 2> repro.err

diff_nocr repro.ref repro.err

synth_only bug

echo "Test successful"
