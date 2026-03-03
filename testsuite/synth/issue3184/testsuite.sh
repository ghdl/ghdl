#! /bin/sh

. ../../testenv.sh

for t in repro3; do
    synth_tb $t
done

analyze repro4_pkg.vhdl repro4.vhdl tb_repro4.vhdl
elab_simulate tb_repro4

clean

synth repro4_pkg.vhdl repro4.vhdl -e > syn_repro4.vhdl
analyze repro4_pkg.vhdl syn_repro4.vhdl tb_repro4.vhdl
elab_simulate tb_repro4

clean

synth -de repro4_pkg.vhdl repro4.vhdl -e > syn_repro4-de.vhdl
analyze repro4_pkg.vhdl syn_repro4-de.vhdl tb_repro4.vhdl
elab_simulate tb_repro4

clean

echo "Test successful"
