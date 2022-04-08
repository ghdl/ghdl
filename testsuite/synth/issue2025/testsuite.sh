#! /bin/sh

. ../../testenv.sh

synth --std=08 wb_standard_axi4_lite_bridge_rtl.vhd wb_standard_axi4_lite_bridge.psl wb_standard_formal_psl.vhd -e wb_standard_axi4_lite_bridge > syn_bridge.vhdl

echo "Test successful"
