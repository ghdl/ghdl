#! /bin/sh

. ../../testenv.sh

#synth -garch=2 -gworks=1 repro_pkg.vhdl repro.vhdl -e > syn_reprod21.vhdl
synth_failure -garch=2 -gworks=0 repro_pkg.vhdl repro.vhdl -e
synth_failure simple3.vhdl -e

echo "Test successful"
