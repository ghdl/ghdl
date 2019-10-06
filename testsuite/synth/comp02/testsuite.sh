#! /bin/sh

. ../../testenv.sh

# Direct instance
analyze pkg.vhdl cmask.vhdl mixer.vhdl tb_mixer.vhdl
elab_simulate tb_mixer
clean

synth pkg.vhdl cmask.vhdl mixer.vhdl -e mixer > syn_mixer.vhdl
analyze syn_mixer.vhdl tb_mixer.vhdl
elab_simulate tb_mixer
clean

echo "Test successful"
