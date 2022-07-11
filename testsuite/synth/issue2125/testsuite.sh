#! /bin/sh

. ../../testenv.sh

# TODO: reuse synth_tb, but we need to pass --latches

analyze afed.vhdl tb_afed.vhdl
elab_simulate tb_afed
clean

synth --latches afed.vhdl -e > syn_afed.vhdl
analyze syn_afed.vhdl tb_afed.vhdl
elab_simulate tb_afed --ieee-asserts=disable-at-0 --assert-level=error
clean

echo "Test successful"
