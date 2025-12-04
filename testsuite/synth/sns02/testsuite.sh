#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys

for t in toint01 id01 neg01 abs01 \
		 cmp01 cmp02 cmp03 cmp04 cmp05 cmp06 cmp07 \
		 add01 sub01 mul01
do
    synth_only $t
done

echo "Test successful"
