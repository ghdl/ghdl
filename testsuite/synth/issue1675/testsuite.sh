#! /bin/sh

. ../../testenv.sh

# accum
analyze pkg.vhdl accum.vhdl tb_accum.vhdl
elab_simulate tb_accum
clean

synth pkg.vhdl accum.vhdl -e > syn_accum.vhdl
analyze pkg.vhdl syn_accum.vhdl tb_accum.vhdl
elab_simulate tb_accum --ieee-asserts=disable-at-0 --assert-level=error
clean

# accumwr
analyze pkg.vhdl accum.vhdl accumwr.vhdl tb_accumwr.vhdl
elab_simulate tb_accumwr
clean

synth pkg.vhdl accum.vhdl accumwr.vhdl -e > syn_accumwr.vhdl
analyze pkg.vhdl syn_accumwr.vhdl tb_accumwr.vhdl
elab_simulate tb_accumwr --ieee-asserts=disable-at-0 --assert-level=error
clean

# patgen
analyze pkg.vhdl patgen.vhdl tb_patgen.vhdl
elab_simulate tb_patgen
clean

synth pkg.vhdl patgen.vhdl -e > syn_patgen.vhdl
analyze pkg.vhdl syn_patgen.vhdl tb_patgen.vhdl
elab_simulate tb_patgen --ieee-asserts=disable-at-0 --assert-level=error
clean

# patacc
analyze pkg.vhdl patgen.vhdl patacc.vhdl tb_patacc.vhdl
elab_simulate tb_patacc
clean

synth pkg.vhdl patgen.vhdl patacc.vhdl -e > syn_patacc.vhdl
analyze pkg.vhdl syn_patacc.vhdl tb_patacc.vhdl
elab_simulate tb_patacc --ieee-asserts=disable-at-0 --assert-level=error
clean

echo "Test successful"
