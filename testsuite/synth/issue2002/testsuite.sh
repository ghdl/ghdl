#! /bin/sh

. ../../testenv.sh

synth --out=verilog bug.vhdl -e > synth_bug.v
diff_nocr synth_bug.ref synth_bug.v

echo "Test successful"
