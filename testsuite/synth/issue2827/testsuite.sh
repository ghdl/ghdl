#! /bin/sh

. ../../testenv.sh

for f in top; do
    synth --out=verilog ${f}.vhdl -e > syn_${f}.v

    if grep -q "0'b" syn_${f}.v; then
	echo "invalid verilog"
	exit 1
    fi
done

echo "Test successful"
