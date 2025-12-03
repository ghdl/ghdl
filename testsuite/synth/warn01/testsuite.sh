#! /bin/sh

. ../../testenv.sh

for f in tobv01 tobv02 toint01 toint02 toint03 toint04; do
    synth_only $f
    synth_failure -Werror ${f}.vhdl -e
done

for f in toint05; do
    synth -fsynopsys ${f}.vhdl -e > /dev/null
    synth_failure -Werror -fsynopsys ${f}.vhdl -e
done

echo "Test successful"
