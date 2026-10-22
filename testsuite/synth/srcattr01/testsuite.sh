#! /bin/sh

. ../../testenv.sh

synth --out=verilog srcattr01.vhdl -e > syn_srcattr01.v

# Locations are emitted as src attributes (FILE:LINE.COL), before user
# attributes.
fgrep -q "(* src=\"srcattr01.vhdl:16.10\" *) (* keep=1'b1 *) wire [7:0] r;" syn_srcattr01.v
fgrep -q "(* src=\"srcattr01.vhdl:24.5\" *) reg [7:0] " syn_srcattr01.v

# On operators (as there is no attribute on continuous assignments).
fgrep -q "r + (* src=\"srcattr01.vhdl:26.16\" *) 8'b00000001;" syn_srcattr01.v
fgrep -q "~(* src=\"srcattr01.vhdl:36.5\" *) r;" syn_srcattr01.v

# On processes.
fgrep -q "(* src=\"srcattr01.vhdl:24.5\" *) always @(posedge clk or posedge rst)" syn_srcattr01.v
fgrep -q "(* src=\"srcattr01.vhdl:34.3\" *) always @*" syn_srcattr01.v

# Check valid verilog
synth syn_srcattr01.v -e > /dev/null

clean

echo "Test successful"
