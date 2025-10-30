#! /bin/sh

. ../../testenv.sh

synth --out=verilog ent.vhdl -e > syn_ent.v

fgrep -q '(* keep="true" *) reg sig' syn_ent.v

echo "Test successful"
