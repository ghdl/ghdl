#! /bin/sh

. ../../testenv.sh

synth --expect-failure --std=08 -fpsl sequencer.vhd blackbox_sequencer.vhd  blackbox_sequencer.psl -e

echo "Test successful"
