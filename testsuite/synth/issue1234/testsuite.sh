#! /bin/sh

. ../../testenv.sh

synth issue.vhdl -e > syn_issue.vhdl

echo "Test successful"
