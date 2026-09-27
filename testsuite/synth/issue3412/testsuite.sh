#! /bin/sh

. ../../testenv.sh

TESTS="
repro1
repro2
repro3
repro4
repro5
"

for t in $TESTS; do
  #  Check the original, unsynthesized design against the testbench first.
  analyze $* ${t}_pkg.vhdl $t.vhdl tb_$t.vhdl
  elab_simulate tb_$t
  clean

  #  ${t}_pkg.vhdl declares the record type package used by the
  #  synthesized output (which re-emits the original entity declaration
  #  verbatim, including its "use work.pkg...all" clause), so it must be
  #  analyzed again here, and $t.vhdl must not be analyzed alongside it:
  #  the synthesized output declares its own "$t" entity, which would
  #  otherwise clash with the one from $t.vhdl when linking against a
  #  native-code backend.
  synth --keep-hierarchy=no ${t}_pkg.vhdl $t.vhdl -e $t > syn_$t.vhdl
  analyze $* ${t}_pkg.vhdl syn_$t.vhdl tb_$t.vhdl
  elab_simulate tb_$t --ieee-asserts=disable-at-0 --assert-level=error
  clean
done

echo "Test successful"
