#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only issue

! grep " next_event_a:" syn_issue.vhdl

echo "Test successful"
