#! /bin/sh

. ../../testenv.sh

synth_tb loop01

# TODO: should be ok
# Static on the first iteration, but not on the second one.
synth_failure loop02.vhdl -e

synth_failure loop03.vhdl -e

synth_tb loop04
synth_tb loop05

synth_failure loop06.vhdl -e

synth_tb loop07

echo "Test successful"
