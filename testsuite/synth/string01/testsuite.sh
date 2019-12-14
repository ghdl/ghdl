#! /bin/sh

. ../../testenv.sh

analyze string01.vhdl tb_string01.vhdl
elab_simulate tb_string01

clean

synth string01.vhdl -e string01 > syn_string01.vhdl

analyze syn_string01.vhdl tb_string01.vhdl
elab_simulate tb_string01

clean

echo "Test successful"
