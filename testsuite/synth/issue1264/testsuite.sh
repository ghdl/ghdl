#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro
clean

synth_analyze issue
synth_analyze repro
synth_analyze issue2
clean

echo "Test successful"
