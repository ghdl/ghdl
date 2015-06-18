ghdl -a -v --work=ieee_proposed ../x_ieee_proposed/src/std_logic_1164_additions.vhdl
ghdl -a -v --work=ieee_proposed ../x_ieee_proposed/src/standard_additions_c.vhdl
ghdl -a -v --work=ieee_proposed ../x_ieee_proposed/src/standard_textio_additions_c.vhdl
ghdl -a -v --work=bitvis_util src93/types_pkg.vhd
ghdl -a -v --work=bitvis_util src93/adaptations_pkg.vhd
ghdl -a -v --work=bitvis_util src93/string_methods_pkg.vhd
ghdl -a -v --work=bitvis_util src93/vhdl_version_layer_pkg.vhd
ghdl -a -v --work=bitvis_util src93/license_open_pkg.vhd
ghdl -a -v --work=bitvis_util src93/methods_pkg.vhd
ghdl -a -v --work=bitvis_util src93/bfm_common_pkg.vhd

ghdl -a --work=bitvis_util tb/partial_test_tb.vhd
ghdl -e --work=bitvis_util partial_test_tb
ghdl -r --work=bitvis_util partial_test_tb