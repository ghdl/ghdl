#! /bin/sh

. ../../testenv.sh

synth repro.vhdl -e > syn_repro.vhdl 2> repro.err

grep report < repro.err > repro.err1

diff_nocr repro.ref repro.err1

synth_only bug

echo "Test successful"
