#! /bin/sh

. ../../testenv.sh

FILES="
psi_common_array_pkg.vhd
psi_common_math_pkg.vhd
psi_common_logic_pkg.vhd

psi_tb_txt_util.vhd
psi_tb_compare_pkg.vhd
psi_tb_activity_pkg.vhd
psi_tb_i2c_pkg.vhd

psi_common_bit_cc.vhd
psi_common_i2c_master.vhd
psi_common_i2c_master_tb.vhd
"

export GHDL_STD_FLAGS="--std=08 -frelaxed"
analyze repro.vhdl
elab_simulate repro

analyze repro2.vhdl
elab_simulate repro2

analyze $FILES
elab_simulate psi_common_i2c_master_tb

clean

echo "Test successful"
