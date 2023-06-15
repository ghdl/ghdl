#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08


synth_only repro2
analyze syn_repro2.vhdl tb_repro2.vhdl

elab_simulate tb_repro2 > tb_repro2.out
diff_nocr tb_repro2.ref tb_repro2.out


synth_only repro1
analyze syn_repro1.vhdl tb_repro1.vhdl

elab_simulate tb_repro1 > tb_repro1.out
diff_nocr tb_repro1.ref tb_repro1.out

clean

echo "Test successful"
