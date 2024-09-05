#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze repro1.vhdl
elab_simulate repro1

analyze repro2.vhdl
elab_simulate repro2

analyze repro3.vhdl
elab_simulate repro3

clean

if false; then
FILES="
fofb_cc_version.vhd
fofb_cc_pkg.vhd
wishbone_pkg.vhd
dot_prod_pkg.vhd
fofb_sys_id_pkg.vhd
fofb_ctrl_pkg.vhd
fofb_tb_pkg.vhd
sim_wishbone.vhd
wb_fofb_shaper_filt_regs_consts_pkg.vhd
xwb_fofb_shaper_filt_tb.vhd
ifc_common_pkg.vhd
wb_fofb_shaper_filt_regs.vhd
xwb_fofb_shaper_filt.vhd
iir_filt.vhd
xwb_register_link.vhd
wb_slave_adapter.vhd
biquad.vhd
wb_skidpad.vhd
"

export GHDL_STD_FLAGS=--std=08
for f in $FILES; do
    analyze $f
done
elab_simulate xwb_fofb_shaper_filt_tb

clean
fi

echo "Test successful"
