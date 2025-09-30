#! /bin/sh

. ../../testenv.sh

for f in ent ent2 ent3 ent4; do
  # synthesis, verilog output and keep only lines for ports
  synth --out=verilog ${f}.vhdl -e | grep "put " > syn_${f}.v
  diff_nocr syn_${f}.v ${f}.ref
done

echo "Test successful"
