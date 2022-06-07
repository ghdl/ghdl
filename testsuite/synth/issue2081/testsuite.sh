#! /bin/sh

. ../../testenv.sh

synth ent.vhdl -e > syn_ent.vhdl

synth_failure -Werror=nowrite ent.vhdl -e

echo "Test successful"
