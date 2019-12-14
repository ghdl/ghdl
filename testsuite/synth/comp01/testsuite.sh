#! /bin/sh

. ../../testenv.sh

# Direct instance
analyze and3.vhdl and6.vhdl tb_and6.vhdl
elab_simulate tb_and6
clean

synth and3.vhdl and6.vhdl -e and6 > syn_and6.vhdl
analyze syn_and6.vhdl tb_and6.vhdl
elab_simulate tb_and6
clean

# Component instance
analyze and3.vhdl and6comp.vhdl tb_and6.vhdl
elab_simulate tb_and6
clean

synth and3.vhdl and6comp.vhdl -e and6 > syn_and6.vhdl
analyze syn_and6.vhdl tb_and6.vhdl
elab_simulate tb_and6
clean

# Black box
synth and6comp.vhdl -e and6 > syn_and6bb.vhdl

echo "Test successful"
