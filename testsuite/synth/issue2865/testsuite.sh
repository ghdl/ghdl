#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth memac_rmii_rx.vhdl memac_rmii_rx.psl -e > syn_memac_rmii_rx.vhdl

echo "Test successful"
