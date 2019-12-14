#! /bin/sh

. ../../testenv.sh

analyze add_142.vhd add_153.vhd add_154.vhd add_155.vhd add_159.vhd cmp_662.vhd cmp_673.vhd cmp_694.vhd cmp_700.vhd compressed.vhd decis_levl.vhd fsm_163.vhd ilb_table.vhd mul_145.vhd mul_146.vhd mul_148.vhd mul_149.vhd mul_156.vhd mul_161.vhd qq2_code2_table.vhd qq4_code4_table.vhd qq6_code6_table.vhd quant26bt_neg.vhd quant26bt_pos.vhd result.vhd shr_141.vhd sub_143.vhd sub_144.vhd sub_147.vhd sub_160.vhd tb.vhd test_data.vhd top.vhd wh_code_table.vhd wl_code_table.vhd

elab_simulate tb

#clean

echo "Test successful"

