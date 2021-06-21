#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="-fsynopsys"

for t in ram1 ram2 ram4; do
    synth_tb $t 2> $t.log
    grep "found R" $t.log
done

synth_analyze ram9 2> ram9.log
grep "found RAM" ram9.log

synth asymmetric_ram_2a.vhd  -e > syn_asymmetric_ram_2a.vhdl 2> ram_2a.log
grep "found RAM" ram_2a.log

echo "Test successful"
