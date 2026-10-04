#! /bin/sh

. ../../testenv.sh

synth pkg.vhdl inner.vhdl top.vhdl -e > syn_top.vhdl

analyze pkg.vhdl syn_top.vhdl

clean

TESTS="
repro1
repro2
repro3
repro4
repro5
"

for t in $TESTS; do
  #  Check the original, unsynthesized design against the testbench first.
  analyze $* pkg.vhdl $t.vhdl tb_$t.vhdl
  elab_simulate tb_$t
  clean

  #  Try synthesized version (with no hierarchy)
  synth pkg.vhdl $t.vhdl -e $t > syn_$t.vhdl
  analyze $* pkg.vhdl syn_$t.vhdl tb_$t.vhdl
  elab_simulate tb_$t --ieee-asserts=disable-at-0 --assert-level=error
  clean

  #  Try synthesized version (with no hierarchy)
  synth --keep-hierarchy=no pkg.vhdl $t.vhdl -e $t > syn_$t.vhdl
  analyze $* pkg.vhdl syn_$t.vhdl tb_$t.vhdl
  elab_simulate tb_$t --ieee-asserts=disable-at-0 --assert-level=error
  clean
done

echo "Test successful"
