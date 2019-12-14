#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate_failure repro

analyze repro64.vhdl
elab_simulate repro64

clean

echo "Test successful"


