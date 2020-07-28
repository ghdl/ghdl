#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro

analyze repro2.vhdl
elab_simulate repro2

clean

GHDL_STD_FLAGS=--work=ieee_proposed
analyze x_ieee_proposed/src/std_logic_1164_additions.vhdl
analyze x_ieee_proposed/src/standard_additions_c.vhdl
analyze x_ieee_proposed/src/standard_textio_additions_c.vhdl

GHDL_STD_FLAGS=--work=bitvis_util
analyze project/src93/types_pkg.vhd
analyze project/src93/adaptations_pkg.vhd
analyze project/src93/string_methods_pkg.vhd
analyze project/src93/vhdl_version_layer_pkg.vhd
analyze project/src93/license_open_pkg.vhd
analyze project/src93/methods_pkg.vhd
analyze project/src93/bfm_common_pkg.vhd

analyze project/tb/partial_test_tb.vhd
elab_simulate_failure partial_test_tb

clean
clean ieee_proposed
clean bitvis_util

rm -f alertlog.txt testlog.txt
echo "Test successful"
