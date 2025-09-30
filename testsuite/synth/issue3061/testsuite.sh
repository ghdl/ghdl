#! /bin/sh

. ../../testenv.sh

for f in ent ent2 ent3 ent4; do
  synth --out=verilog ${f}.vhdl -e > syn_${f}.v
  diff_nocr syn_${f}.v ${f}.ref
done

echo "Test successful"
