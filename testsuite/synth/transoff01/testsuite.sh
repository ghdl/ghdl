#! /bin/sh

. ../../testenv.sh

verilog_synth_tb translate_off1

#verilog_run translate_off2.v tb_translate_off2.v
synth --out=verilog translate_off2.v -e > syn_translate_off2.v
#verilog_run syn_translate_off2.v tb_syn_translate_off2.v

echo "Test successful"
