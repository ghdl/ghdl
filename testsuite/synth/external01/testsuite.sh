#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
GHDL_SYNTH_FLAGS=--keep-hierarchy=no

synth_only external01
synth_only external02
synth_only external03
synth_only external04

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
synth_failure $GHDL_SYNTH_FLAGS externalerr09.vhdl -e
synth_failure $GHDL_SYNTH_FLAGS externalerr10.vhdl -e
synth_failure $GHDL_SYNTH_FLAGS externalerr11.vhdl -e
synth_failure $GHDL_SYNTH_FLAGS externalerr12.vhdl -e
synth_failure $GHDL_SYNTH_FLAGS externalerr13.vhdl -e
synth_failure $GHDL_SYNTH_FLAGS externalerr14.vhdl -e
synth_failure $GHDL_SYNTH_FLAGS externalerr15.vhdl -e

echo "Test successful"
