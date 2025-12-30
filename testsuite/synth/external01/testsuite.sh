#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
GHDL_SYNTH_FLAGS=--keep-hierarchy=no

synth_only external01
synth_only external02
synth_only external03

# Without --keep-hierarchy
# TODO: improve error message
synth_failure external02.vhdl -e

synth_failure externalerr01.vhdl -e
synth_failure externalerr03.vhdl -e
synth_failure externalerr04.vhdl -e
synth_failure externalerr05.vhdl -e
synth_failure externalerr06.vhdl -e
synth_failure $GHDL_SYNTH_FLAGS externalerr07.vhdl -e
synth_failure $GHDL_SYNTH_FLAGS externalerr08.vhdl -e

echo "Test successful"
