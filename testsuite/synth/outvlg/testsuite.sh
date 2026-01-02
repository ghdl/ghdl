#! /bin/sh

. ../../testenv.sh

synth --out=verilog const01.vhdl -e > syn_const01.v
synth --out=verilog const02.vhdl -e > syn_const02.v
synth --out=verilog rom01.vhdl -e > syn_rom01.v
synth --out=verilog dff08.vhdl -e > syn_dff08.v

synth --out=verilog --std=08 oper01.vhdl -e > syn_oper01.v
synth --out=verilog --std=08 oper02.vhdl -e > syn_oper02.v
synth --out=verilog --std=08 oper03.vhdl -e > syn_oper03.v

synth --out=verilog port01.vhdl -e > syn_port01.v

echo "Test successful"
