#! /bin/sh

. ../../testenv.sh

synth_only sipo

synth --out=raw sipo.vhdl -e > sipo.raw

CNT=$(grep -c dff sipo.raw)
test $CNT -eq 2

echo "Test successful"
