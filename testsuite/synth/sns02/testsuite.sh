#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys

for t in conv01 conv02 conv03 \
	 id01 neg01 abs01 \
	 cmp01 cmp02 cmp03 cmp04 cmp05 cmp06 cmp07 cmp08 \
	 add01 sub01 mul01 \
	 ext01 shift01 reduce01
do
    synth_only $t

    analyze $t.vhdl
    elab_simulate $t
done

echo "Test successful"
