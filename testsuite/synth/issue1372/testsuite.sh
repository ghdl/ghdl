#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth issue.vhdl issue_psl.vhdl -e > syn_issue.vhdl

echo "Test successful"
