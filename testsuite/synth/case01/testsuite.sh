#! /bin/sh

. ../../testenv.sh

for t in case01 case02 case03 case04; do
    synth_tb $t
done

synth case01.v -e > syn_case01.vhdl
analyze syn_case01.vhdl tb_case01.vhdl
elab_simulate tb_case01 --ieee-asserts=disable-at-0 --assert-level=error

for t in case05 case06 case07; do
  synth_only $t
done

echo "Test successful"
