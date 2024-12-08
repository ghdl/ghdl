#! /bin/sh

. ../../testenv.sh

for f in top topsub; do
    synth --out=verilog ${f}.vhdl -e > syn_${f}.v

    if grep -q downto syn_${f}.v; then
	echo "invalid verilog"
	exit 1
    fi
done

echo "Test successful"
