#! /bin/sh

. ../../testenv.sh

#! /bin/sh

. ../../testenv.sh

analyze and3.vhdl and6.vhdl tb_and6.vhdl
elab_simulate tb_and6
clean

synth and3.vhdl and6.vhdl -e and6 > syn_and6.vhdl
analyze syn_and6.vhdl tb_and6.vhdl
elab_simulate tb_and6
clean

echo "Test successful"
